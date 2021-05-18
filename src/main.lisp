(in-package :mcutiet)


;; (defclass mqtt-header ()
;;   ((package-type :initarg :type :accessor package-type )
;;    (flags :initarg :flags :accessor flags)))

;; (defparameter *stream*
;;   (open "mcutiet/data/connect.bin" :element-type '(unsigned-byte 8)))

;; (defun read-u1 (in)
;;   (read-byte in))

;; (defun read-bits (from number pos)
;;   (ldb (byte number pos) from))

;; (defun read-fixed-header (in)
;;   (let ((tag (make-instance 'mqtt-header)))
;;     (with-slots (package-type flags) tag
;;       (setf package-type   (read-u1 in))
;;       (setf flags (read-u1 in)))
;;     tag))

;; (defparameter *header*  0)

;; ;; (setq *header*  (with-open-file (stream "mqtt-client/data/connect.bin" :if-does-not-exist nil :element-type '(unsigned-byte 8)) 
;; ;;                   (read-fixed-header stream)))




;; (defclass fixed-header ()
;;   ((header
;;     :type '(unsigned-byte 8)
;;     :initarg :header
;;     :initform 0
;;     :accessor header)
;;    (remaining-length
;;     :initarg :remaining-length
;;     :initform 0
;;     :accessor remaining-length)))

;; (defclass packet-identifier (fixed-header)
;;   ((packet-identifier-msb
;;     :type '(unsigned-byte 8)
;;     :initarg :packet-identifier-msb
;;     :initform 0
;;     :accessor packet-identifier-msb)
;;    (packet-identifier-lsb
;;     :type '(unsigned-byte 8)
;;     :initarg :packet-identifier-lsb
;;     :initform 0
;;     :accessor packet-identifier-lsb)))

;; (defclass connect (fixed-header)
;;   ((protocol-name
;;     :type 'string
;;     :initform "MQTT"
;;     :reader protocol-name)
;;    (protocol-level
;;     :type '(unsigned-byte 8)
;;     :initform 4
;;     :reader protocol-level)
;;    (connect-flags
;;     :type '(unsigned-byte 8)
;;     :initarg :connect-flags
;;     :initform 0
;;     :reader connect-flags)
;;    (keep-alive
;;     :type '(unsigned-byte 16)
;;     :initarg :keep-alive
;;     :initform 0
;;     :accessor keep-alive)
;;    (client-id
;;     :type 'string
;;     :initarg :client-id
;;     :initform ""
;;     :accessor client-id)))

;; (defgeneric packet-type (fixed-header)
;;   (:documentation "reader for packet type"))

;; (defmethod packet-type ((header fixed-header))
;;   (let ((id ))
;;     (setf id (ldb (byte 4 4) (slot-value header 'header)))
;;     (packet-type-from-raw id)))

;; (defgeneric (setf packet-type) (value fixed-header)
;;   (:documentation "writer for packet type"))

;; (defmethod (setf packet-type) (value (header fixed-header))
;;   (setf (ldb (byte 4 4) (slot-value header 'header)) (packet-type-to-raw value)))

;; (defgeneric qos (fixed-header)
;;   (:documentation "reader for qos flag from mqtt spec"))

;; (defmethod qos ((header fixed-header))
;;   (let ((pos))
;;     (setf pos (ldb (byte 2 1) (slot-value header 'header)))
;;     (qos-from-raw pos)))

;; (defgeneric (setf qos) (value fixed-header)
;;   (:documentation "writer for qos flag"))

;; (defmethod (setf qos) (value (header fixed-header))
;;   (setf (ldb (byte 2 1) (slot-value header 'header)) (qos-to-raw value)))

;; (defgeneric dup-p (fixed-header)
;;   (:documentation "reader for dup flag according to mqtt spec"))

;; (defmethod dup-p ((header fixed-header))
;;   (not (zerop (ldb (byte 1 3) (slot-value header 'header)))))

;; (defgeneric (setf dup) (value fixed-header)
;;   (:documentation "write for dup flag according to mqtt spec"))

;; (defmethod (setf dup) (value (header fixed-header))
;;   (setf (ldb (byte 1 3) (slot-value header 'header)) (if value 1 0) ))

;; (defgeneric retain-p (fixed-header)
;;   (:documentation "reader for dup flag according to mqtt spec"))

;; (defmethod retain-p ((header fixed-header))
;;   (not (zerop (ldb (byte 1 0) (slot-value header 'header)))))

;; (defgeneric (setf retain) (value fixed-header)
;;   (:documentation "writer for dup flag according to mqtt spec"))

;; (defmethod (setf retain) (value (header fixed-header))
;;   (setf (ldb (byte 1 0) (slot-value header 'header)) (if value 1 0) ))

;; (defgeneric clean-session-p (connect)
;;   (:documentation "reader for clean session flag"))

;; (defmethod clean-session-p ((object connect))
;;   (not (zerop (ldb (byte 1 1) (slot-value object 'connect-flags)))))

;; (defgeneric (setf clean-session) (value conect)
;;   (:documentation "writer for clean session flag"))

;; (defmethod (setf clean-session) (value (object connect))
;;   (setf (ldb (byte 1 1) (slot-value object 'connect-flags))
;;         (if value 1 0)))

;; (defgeneric will-flag-p (connect)
;;   (:documentation "reader for will flag"))

;; (defmethod will-flag-p ((object connect))
;;   (not (zerop (ldb (byte 1 2) (slot-value object 'connect-flags)))))

;; (defgeneric (setf will-flag) (value conect)
;;   (:documentation "writer for clean session flag"))

;; (defmethod (setf will-flag) (value (object connect))
;;   (setf (ldb (byte 1 2) (slot-value object 'connect-flags))
;;         (if value 1 0)))

;; (defgeneric will-qos (connect)
;;   (:documentation "reader for will qos flag"))

;; (defmethod will-qos ((object connect))
;;   ( ldb (byte 2 3) (slot-value object 'connect-flags)))

;; (defgeneric (setf will-qos) (value conect)
;;   (:documentation "writer for will qos flag"))

;; (defmethod (setf will-qos) (value (object connect))
;;   (setf (ldb (byte 2 3) (slot-value object 'connect-flags)) value))

;; (defgeneric will-retain-p (connect)
;;   (:documentation "reader for will retain flag"))

;; (defmethod will-retain-p ((object connect))
;;   (not (zerop  ( ldb (byte 1 5) (slot-value object 'connect-flags)))))

;; (defgeneric (setf will-retain) (value conect)
;;   (:documentation "writer for will qos flag"))

;; (defmethod (setf will-retain) (value (object connect))
;;   (setf (ldb (byte 1 5) (slot-value object 'connect-flags))
;;         (if value 1 0 )))

;; (defgeneric password-flag-p (connect)
;;   (:documentation "reader for will qos flag"))

;; (defmethod password-flag-p ((object connect))
;;   (not (zerop ( ldb (byte 1 6) (slot-value object 'connect-flags)))))

;; (defgeneric (setf password-flag) (value conect)
;;   (:documentation "writer for will qos flag"))



;; (defgeneric username-flag-p (connect)
;;   (:documentation "reader for the username flag"))

;; (defmethod username-flags-p ((object connect))
;;   (not (zerop ( ldb (byte 1 7) (slot-value object 'connect-flags)))))

;; (defgeneric (setf username-flag) (value conect)
;;   (:documentation "writer for the password flag"))

;; (defmethod (setf username-flags) (value (object connect))
;;   (setf (ldb (byte 1 7) (slot-value object 'connect-flags))
;;         (if value 1 0 )))

;; ;; (defparameter *test* (make-instance 'fixed-header :header #b10100000))

;; ;; (defparameter *connect* (make-instance 'connect))

;; ;; (defparameter *encoded-bytes*  (encode-size 268435455))

;; ;; (defparameter *byte-stream* (make-instance 'octet-input-stream
;; ;;                                            :data *encoded-bytes*))

;; ;; (defparameter *test-stream* (make-array 0 :element-type '(unsigned-byte)
;; ;;                                           :adjustable t
;; ;;                                           :fill-pointer t))

;; ;; (defparameter *byte-output-stream* (make-instance
;; ;;                                     'octet-bidirectional-stream
;; ;;                                     :data (make-array 0 :element-type '(unsigned-byte 8)
;; ;;                                                       :adjustable t
;; ;;                                                       :fill-pointer t)))

;; ;; (defparameter *byte-output-stream* (make-instance
;; ;;                                     'octet-bidirectional-stream))


;; ;; (defparameter *decoded-bytes* (decode-size *byte-stream*))


;; (defun write-u2 (value stream)
;;   (write-byte (ldb (byte 8 8) value) stream)
;;   (write-byte (ldb (byte 8 0) value) stream))

;; (defun write-u1 (value stream)
;;   (write-byte value stream))

;; (defun write-bytes (values stream)
;;   (loop for value across values
;;         do (write-byte value stream)))

;; (defun write-mqtt-string (values stream)
;;   (write-u2 (length values) stream)
;;   (write-bytes values stream))

;; (defun create-connect (client-id)
;;   (let ((temp (make-instance 'connect :header 0 :client-id client-id)))
;;     (setf (packet-type temp) :connect)
;;     (setf (keep-alive temp) 100)
;;     (setf (clean-session temp) t)
;;     temp))


;; (defun write-connect (object stream)
;;   (with-slots (header
;;                remaining-length
;;                protocol-name
;;                protocol-level
;;                connect-flags
;;                keep-alive
;;                client-id) object
;;     (write-u1 header stream)
;;     (write-bytes (encode-size
;;                   (+ (length (babel:string-to-octets client-id)) ;client-id
;;                      (length (babel::string-to-octets protocol-name)) ;protocol-name
;;                      1                  ;protocol-level
;;                      1                  ;connect-flags
;;                      2                  ;keep-alive
;;                      2                  ;protocol-name size
;;                      2                  ;client-id-size
;;                      )) stream)
;;     (write-mqtt-string (babel:string-to-octets protocol-name) stream)
;;     (write-u1 protocol-level stream)
;;     (write-u1 connect-flags stream)
;;     (write-u2 keep-alive stream)
;;     (write-mqtt-string (babel:string-to-octets client-id) stream))
;;   stream)

;; (declaim (optimize (speed 0) (space 0) (debug 3)))

;; (defun test ()
;;   (as:tcp-connect "mosquitto" 1883
;;                   (lambda (socket data)
;;                     (let* ((header (read-byte data nil))
;;                            (length (decode-size data))
;;                            (rest (make-array length)))
;;                       (read-sequence rest data)
;;                       (format t "Header: ~x~%" header)
;;                       (format t "Length: ~x~%" length)
;;                       (print rest)))
;;                   (lambda (ev) (format t "ev: ~a~%" ev))
;;                   :data (babel:octets-to-string (slot-value *connect-pack* 'data))
;;                   :stream t))

;; ;; (defparameter *bytes* (make-instance 'octet-bidirectional-stream
;; ;;                                      :data (make-array 0 :element-type '(unsigned-byte 8)
;; ;;                                                          :adjustable t
;; ;;                                                          :fill-pointer
;; ;;                                                          t)))

;; ;; (defparameter *connect-pack* (write-connect (create-connect "lisp mqtt") *bytes*))


;; ;; (as:start-event-loop (lambda () (test)))


;; ;; (defparameter *simple-bytes* (make-array (length (slot-value *connect-pack* 'data)) :initial-contents (slot-value *connect-pack* 'data) :element-type '(unsigned-byte 8)))

;; ;; (defparameter *socket* (usocket:socket-connect "mosquitto" 1883
;; ;;                                                :element-type '(unsigned-byte 8)))

;; (defclass client ()
;;   ((socket :initarg :socket :accessor socket)
;;    (client-id :initarg :client-id :reader client-id)))

;; (defun read-message (socket)
;;   (let* ((header (read-byte (usocket:socket-stream socket)))
;;          (length (decode-size (usocket:socket-stream socket)))
;;          (rest (make-array length)))
;;     (read-sequence rest (usocket:socket-stream socket))
;;     (format t "Header: ~x~%" header)
;;     (format t "Length: ~x~%" length)
;;     (print rest)))

;; (defun write-message (socket message)
;;   (write-sequence message (usocket:socket-stream socket))
;;   (force-output (usocket:socket-stream socket)))

;; ;; (defun connect (host port client-id &key user pass clean-session)
;; ;;   (let ((buffer (make-instance 'octet-bidirectional-stream
;; ;;                                :data (make-array 0
;; ;;                                                  :element-type '(unsigned-byte 8)
;; ;;                                                  :adjustable t
;; ;;                                                  :fill-pointer t)))
;; ;;         (socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
;; ;;     (write-message socket (slot-value (write-connect (create-connect client-id) buffer) 'data))
;; ;;     (read-message socket)
;; ;;     (make-instance 'client :socket socket :client-id client-id)))


;; (defparameter *mcutiet* (connect "mosquitto" 1883 "lisp mqtt" :clean-session t))
;; (usocket:socket-send *socket* *simple-bytes* (length *simple-bytes*))

;; (setf (packet-type *test*) :subscribe)
;; (setf (qos *test*) :reserved )
;; (setf (dup *test*) nil )
;; (setf (retain *test*) nil)

;; (setf (header *connect*) #b10100000)

;; (qos *test*)
;; (dup-p *test*)
;; (retain-p *test*)
;; (setf (qos *test*) :reserved )
;; (setf (will-flag *connect*) 1)
;; (setf (will-qos *connect*) 2)
;; (will-qos *connect*)
;; (will-flag-p *connect*)
;; (packet-type *test*)

;; (qos *connect*)
;; (babel:octets-to-string )
;; (babel:string-to-octets "MQTT" :encoding :utf-8)


;; (setf (clean-session *connect*) t)
;; (protocol-name *connect*)


;; ;; (babel:sr)
;; ;; babel:string-to-octets
;; ;;                                       ,value-expr
;; ;;                                       :encoding :utf-8)



;; ;;;;;;;;;;;;;;;;;;;

;; ;; Type definitions for mqtt protocoll

(defparameter *connect-pkt*
  (connect-factory "lispmqtt" 30
                   :user "test"
                   :password "yeah"
                   :will-topic "/home/icepic/test"
                   :will-message "I am dead"))

(defparameter *publish-pkt*
  (publish-factory
   "/home/icepic/test" (babel:string-to-octets "Temp 100: Hum 40")))

(defparameter *pingreq-pkt*
  (pingreq-factory ))

(defparameter *disconnect-pkt*
  (disconnect-factory))

(defparameter *subscripe-pkt* (subsribe-factory "/home/icepic/test"))

(defparameter *client* (connect-to-server "mosquitto" 1883 "lispmqtt" 30
                                          :will-topic "/home/icepic/test"
                                          :will-message "I die" ))

(subscribe *client* "/home/icepic/test")

(subscribe *client* "/home/office/christoph/sht85/indoor")
(read-messages *client*
               (lambda (pkt)
                 (format t "Received: ~a~%" (type-of pkt))
                 (format t "Payload: ~a~%" (babel:octets-to-string (payload pkt)))
                 t))

(ping *client*)

(disconnect *client*)

(write-message (socket *client*) *pingreq-pkt*)
(defparameter *pingresp-pkt* (read-message (socket *client*)))

(write-message (socket *client*) *subscripe-pkt*)

(write-message (socket *client*) *publish-pkt*)
(write-message (socket *client*) *disconnect-pkt*)



(defparameter *bytes* (make-instance 'octet-bidirectional-stream
                                     :data (make-array 0 :element-type '(unsigned-byte 8)
                                                         :adjustable t
                                                         :fill-pointer
                                                         t)))


(defparameter *socket* (usocket:socket-connect "mosquitto" 1883 :element-type '(unsigned-byte 8)))

(defparameter *stream* (read-write *socket* *packet*))



(binary-data:write-value 'fixed-header *bytes* *packet*)

(defparameter *test*  (binary-data:read-value 'fixed-header *bytes*))

(write-message *socket* (slot-value *bytes* 'data))

(defparameter bla (binary-data:read-value 'fixed-header *stream*))

;; (defparameter bla2 (binary-data:write-value 'new-fixed-header *bytes* *packet*))

;; (defparameter bla3 (binary-data:read-value 'new-fixed-header *bytes*))


;; (read-ue 'fixed-header *bytes*)


;; (binary-data:read-value 'generic-string *bytes* :length 3)

;; (create-connect "dfdf")

;; ;;;;;;;;;;;;;;;; 
