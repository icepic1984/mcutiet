(in-package :cl)

(defpackage :mcutiet.client
  (:use
   :cl
   :mcutiet.message
   :mcutiet.io
   :mcutiet.streams
   :mcutiet.queue)
  (:local-nicknames (#:a #:alexandria))
  (:export
   :run-once
   :connect-to-server
   :disconnect-from-server
   :send-connect
   :wait-for-connack
   :subscribe
   :wait-for-suback
   :process-message))

(in-package :mcutiet.client)

(defun current-time-in-sec ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun is-answer-quick-enough (last-time current-time &optional (timeout 3))
  (> timeout (- current-time last-time)))

(defun time-for-next-ping-is-up (last-time current-time keep-alive &optional (ratio 3/4))
  ;; Check if time between last and current time is larger than 3/4 of
  ;; keep-alive time. If so, a new ping request will be send.
  (if (> (- current-time last-time)
         (* ratio keep-alive))
      t nil))

(defclass mcutiet-client ()
  ((client-id :initarg :client-id :reader client-id)
   (keep-alive :initarg :keep-alive :reader keep-alive)
   (socket :initarg :socket :reader socket)
   (socket-stream :initarg :socket-stream :reader socket-stream)
   (host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (connection-established :initform nil :reader connection-established)
   (pending-connack :initform nil :reader pending-connack)
   (pending-ping :initform nil :reader pending-ping)
   (last-ping :initform (current-time-in-sec) :reader last-ping)))


(defun connect-to-server (host port &key
                                      (client-id "mcutiet")
                                      (keep-alive 300))
  (let ((client (make-instance 'mcutiet-client
                               :host host
                               :port port
                               :client-id client-id
                               :keep-alive keep-alive)))
    (with-slots (socket socket-stream) client
      (setf socket (usocket:socket-connect
                    host port
                    :element-type '(unsigned-byte 8)
                    :timeout 3))
      (setf socket-stream (usocket:socket-stream socket)))
    client))

(defun disconnect-from-server (client)
  (with-slots (connection-established) client
    (usocket:socket-close (socket client))
    (setf connection-established nil)))


(defun send-connect (client &key
                              (clean-session t)
                              (username nil)
                              (password nil)
                              (will-topic nil)
                              (will-message nil)
                              (will-qos :at-most-once)
                              (will-retain nil))

  (assert (not (connection-established client)))
  (assert (not (pending-connack client)))
  (let ((packet (make-instance 'connect
                               :client-id (client-id client)
                               :keep-alive (keep-alive client)
                               :clean-session clean-session
                               :username username
                               :password password
                               :will-topic will-topic
                               :will-message will-message
                               :will-qos will-qos
                               :will-retain will-retain)))
    (throw-if-invalid packet)
    (write-sequence (message-to-octets packet) (socket-stream client))
    (force-output (usocket:socket-stream (socket client)))
    (with-slots (pending-connack) client
      (setf pending-connack t))))

(defun wait-for-connack (client)
  (assert (pending-connack client))
  (assert (not (connection-established client)))
  (let ((message
          (octets-to-message (bytes (read-raw-message-with-timeout (socket-stream client))))))
    (assert (eql (packet-type message) :connack))
    (assert (eql (connect-return-code message) :accepted))
    (with-slots (pending-connack connection-established) client
      (setf pending-connack nil)
      (setf connection-established t))))

(defun send-ping (client)
  (format t "Ping req~%")
  (assert (connection-established client))
  (let ((packet (make-instance 'pingreq)))
    (throw-if-invalid packet)
    (write-sequence (message-to-octets packet) (socket-stream client))
    (force-output (usocket:socket-stream (socket client)))
    (with-slots (pending-ping) client
      (setf pending-ping (current-time-in-sec)))))

(defun wait-for-pingresp (client)
  (assert (pending-ping client))
  (let ((message
          (octets-to-message (bytes (read-raw-message-with-timeout (socket-stream client))))))
    (assert (eql (packet-type message) :pingresp))
    (with-slots (pending-ping last-ping) client
      (setf last-ping (current-time-in-sec))
      (setf pending-ping nil))))


(defun subscribe (client topic-filter &key
                                        (requested-qos :at-most-once))
  (assert (connection-established client))
  (let ((packet (make-instance 'subscribe
                               :topic-filter topic-filter
                               :requested-qos requested-qos
                               :packet-identifier 1)))
    (throw-if-invalid packet)
    (write-sequence (message-to-octets packet) (socket-stream client))
    (force-output (usocket:socket-stream (socket client)))))

(defun wait-for-suback (client)
  (let ((message
          (octets-to-message (bytes (read-raw-message-with-timeout (socket-stream client))))))
    (assert (eql (packet-type message) :suback))))

(defun ping-required (client)
  ;; If the connection is established, we currently
  ;; don't have another pending ping request and the time is nearly up
  ;; for another request, a new ping request is required.
  (if (and
       (connection-established client)
       (not (pending-ping client))
       (time-for-next-ping-is-up (last-ping client) (current-time-in-sec) (keep-alive client)))
      t nil))

(defgeneric process-message (message client)
  (:documentation "Invoke processing of message."))

(defmethod process-message ((object fixed-header) client)
  (format t "Received: ~A~%" (packet-type object)))

(defmethod process-message ((object pingresp) client)
  (assert (pending-ping client))
  (with-slots (last-ping pending-ping) client
    ;; Reset pending ping status and update time
    (setf pending-ping nil)
    (setf last-ping (current-time-in-sec)))
  (format t "Ping resp~%"))

(defmethod process-message ((object connack) client)
  (assert (not (connection-established client)))
  (assert (pending-connack client))
  (assert (eql (connect-return-code object) :accepted))
  (with-slots (pending-connack connection-established) client
    (setf pending-connack nil)
    (setf connection-established t)))

(defmethod process-message ((object publish) client)
  (assert (connection-established client))
  (format t "Topic: ~A~%" (topic-name object))
  (format t "Payload: ~A~%" (babel:octets-to-string (payload object))))

(defun run-once (client)

  ;; If ping is required, send new ping
  (when (ping-required client)
    (send-ping client))

  ;; Check if ping response came quick enough
  (a:when-let ((time (pending-ping client)))
    (unless (is-answer-quick-enough time (current-time-in-sec))
      (error 'read-timeout :timeout 3)))

  ;; Process new messages
  (a:when-let ((incoming-message (read-incoming-message-no-hang (socket-stream client))))
    (let ((message (octets-to-message (bytes incoming-message))))
      (process-message (octets-to-message (bytes incoming-message)) client)
      message)))

(defun start (host port topic keep-alive retries)
  (loop for i from 1 to retries do
    (restart-case
        (let ((client (connect-to-server host port :keep-alive keep-alive)))
          (send-connect client)
          (wait-for-connack client)
          (subscribe client topic)
          (wait-for-suback client)
          (loop do
            (sleep 0.5)
            (run-once client)))
      (retry () :report "Reconnect and continue" nil))
    (format t "Retry~%")
    (sleep 1))
  (format t "Finished!~%"))






;; (handler-bind ((usocket:connection-refused-error (lambda (c)
;;                         (declare (ignore c))
;;                         (format t "Connection refused. Retry~%")
;;                         (invoke-restart 'retry))))
;;   (start "kube-master" 30000 "/home/office/outdoor" 20 5))

;(start "kube-master" 30000 "/home/office/outdoor" 20)
;; Example run-once loop
;; (progn
;;   (defparameter *client* (connect-to-server "kube-mastr" 30001 :keep-alive 20))
;;   (send-connect *client*)
;;   (wait-for-connack *client*)
;;   (subscribe *client* "/home/office/outdoor")
;;   (wait-for-suback *client*)
;;   (loop for i from 1 to 100
;;         do
;;            (sleep 1)
;;            (run-once *client*))
;;   (disconnect-from-server *client*))




