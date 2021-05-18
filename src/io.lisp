;; Copyright (c) 2020, Christoph Buck. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED

(in-package :cl)

(defpackage :mcutiet.io
  (:use
   :cl
   :mcutiet.message
   :mcutiet.streams)
  (:export
   :vector-push-extend-seq
   :read-sequence-no-hang
   :read-message
   :write-message
   :read-message-from-file
   :write-message-to-file
   :message-to-octets
   :octets-to-message
   :message-to-input-stream
   :input-stream-to-message
   :read-raw-message-no-hang
   :read-incoming-message-no-hang
   :read-raw-message-with-timeout
   :reset-incoming-message
   :read-raw-message
   :read-timeout
   ))

(in-package :mcutiet.io)

(defun read-raw-message (stream)
  "Reads mqtt message from stream without parsing content. Returns raw
array of bytes."
  (let* ((header (read-byte stream))
         (size (decode-size stream))
         (encoded-size (encode-size size))
         (bytes (make-array (+ size 1 (length encoded-size))
                            :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream :start (+ 1 (length encoded-size)))
    (setf (elt bytes 0) header)
    (setf (subseq bytes 1 (+ 1 (length encoded-size))) encoded-size)
    bytes))

(defun read-raw-message-no-hang (stream message)

  ;; Check if at least one byte is ready
  (unless (listen stream)
    (return-from read-raw-message-no-hang nil))

  ;; Check if no bytes were read yet. If so, try reading first byte
  ;; from stream (this is the header) and push it back into the output
  ;; array.
  (when (= 0 (length (bytes message)))
    (vector-push-extend (read-byte stream) (bytes message)))

  ;; Try parsing the remaining packet length from message. State is
  ;; saved to `message`. If `decode-size-no-hang` returns true, we are
  ;; finished parsing the remaining length. If false, no bytes were
  ;; avaialble yet and we return the function.
  (when (decode-size-no-hang (remaining-length message) stream)
    (with-slots (bytes-left) (remaining-length message)

      ;; Check if we already append the remaining length back to the
      ;; output buffer. If not our output buffer size is still equal
      ;; to 1. We need to allocate enough space to save the remaining
      ;; bytes from the packet + the encoded remaining length + 1 byte
      ;; for the header.
      (when (= 1 (length (bytes message)))
        (vector-push-extend-seq (bytes message) (encode-size bytes-left))
        (adjust-array (bytes message) (+ (length (bytes message)) bytes-left))
        (setf (fill-pointer (bytes message)) (array-total-size (bytes message))))

      ;; Try reading the remaining bytes to the output buffer
      (let ((read-bytes (read-sequence-no-hang
                         (bytes message) stream
                         :start (- (length (bytes message)) bytes-left)
                         :end (length (bytes message)))))

        (setf bytes-left (- bytes-left read-bytes)))

      ;; If no more bytes are left, we are done and return true.
      (when (= 0 bytes-left)
        (return-from read-raw-message-no-hang t))))
  ;; If we reach here, we still have work to do.
  nil)

(define-condition read-timeout (error)
  ((%timeout :initarg :timeout :reader timeout))
  (:report (lambda (c stream)
             (format stream "Operation timeout after ~A sec" (timeout c)))))

(defun read-raw-message-with-timeout (stream &optional (timeout 3))
  (let ((start-time (/ (get-internal-real-time)
                       internal-time-units-per-second))
        (rl (make-instance 'incoming-message)))
    (loop
      with current-time = (/ (get-internal-real-time)
                             internal-time-units-per-second)
      while (not (read-raw-message-no-hang stream rl))
      do
         (when (< timeout (- current-time start-time))
           (error 'read-timeout :timeout timeout))
         (setf current-time (/ (get-internal-real-time)
                               internal-time-units-per-second)))
    rl))

(let ((incoming-message (make-instance 'incoming-message)))
  (defun reset-incoming-message ()
    (setf incoming-message (make-instance 'incoming-message)))

  (defun read-incoming-message-no-hang (stream)
    (let ((tmp incoming-message))
      (if (read-raw-message-no-hang stream incoming-message)
          (prog2
              (setf incoming-message (make-instance 'incoming-message))
              tmp)
          nil))))

(defun read-message (stream)
  "Read mqtt message from stream."
  (read-value 'fixed-header stream))

(defun write-message (stream object)
  "Write mqtt message to stream."
  (write-value 'fixed-header stream object)
  stream)

(defun read-message-from-file (path)
  "Read mqtt message from file."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (read-message (make-instance
                   'vector-input-stream
                   :data (read-raw-message in) :displaced t))))

(defun write-message-to-file (path object)
  "Write mqtt message to file."
  (with-open-file (out path :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-does-not-exist :create
                            :if-exists :overwrite)
    (write-message out object)))

(defun message-to-octets (object)
  "Convert mqtt message to byte array."
  (let ((stream (make-instance 'vector-output-stream)))
    (write-message stream object)
    (data stream)))

(defun octets-to-message (data)
  "Convert byte array to mqtt message. "
  (let ((stream (make-instance 'vector-input-stream :data data :displaced t)))
    (read-message stream)))

(defun message-to-input-stream (message)
  (make-instance 'vector-input-stream :data (message-to-octets message) :displaced t))

(defun input-stream-to-message (stream)
  (octets-to-message (data stream)))

(defun read-sequence-no-hang (seq stream &key (start 0) (end (length seq)))
  (loop
    for i from start below end
    with num-bytes-read = 0
    while (listen stream)
    do (setf (aref seq i) (read-byte stream))
       (incf num-bytes-read)
    finally (return num-bytes-read)))

(defun vector-push-extend-seq (array src)
  (loop for i from 0 below (length src)
        do (vector-push-extend (elt src i) array)))


;; (read-message-from-file "c:/Projects/git/smarthome/code/local-projects/mqtt-client/data/connect.bin")

;; (write-message-to-file "test.bin" (make-instance 'connect :client-id "bla"))
;; (read-message-from-file "test.bin")

;; (octets-to-message (message-to-octets (make-instance 'connect :client-id "bla")))
