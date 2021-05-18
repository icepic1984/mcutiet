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

(in-package :cl-user)

(defpackage :mcutiet.message
  (:use :cl)
  (:import-from
   :mcutiet.binary-parser
   :define-binary-class
   :define-tagged-binary-class
   :define-binary-type
   :read-value
   :write-value)
  (:import-from
   :babel
   :string-to-octets
   :octets-to-string)
  (:import-from
   :mcutiet.streams
   :vector-input-stream
   :vector-output-stream
   :vector-bidirectional-stream
   :data)
  (:export
   ;; messages
   :fixed-header
   :connect
   :connack
   :publish
   :puback
   :pubrec
   :pubrel
   :pubcomp
   :subscribe
   :suback
   :unsubscribe
   :unsuback
   :pingreq
   :pingresp
   :disconnect
   ;; message field getter / setter
   :packet-type
   :setf packet-type
   :will-flag-p
   :setf will-flag
   :will-qos
   :setf will-qos
   :will-retain-p
   :setf will-retain
   :user-name-p
   :setf user-name-flag
   :password-p
   :setf password-flag
   :clean-session-p
   :setf clean-session
   :session-present-p
   :setf session-present
   :connect-return-code
   :setf connect-return-code
   :qos
   :setf qos
   :dup-p
   :setf dup
   :retain-p
   :setf retain
   :requested-qos
   :setf requested-qos
   :suback-return-code
   :setf suback-return-code
   :topic-filter
   :topic-name
   :payload
   :username
   :password
   ;; message constructors
   :valid-packet
   :throw-if-invalid
   ;; message reader / write
   :read-raw-message
   :read-message
   :write-message
   :read-message-from-file
   :write-message-to-file
   :octets-to-message
   :message-to-octets
   :decode-size
   :encode-size
   :remaining-length
   :incoming-message
   :bytes
   :bytes-left
   :decode-size-no-hang
   :bytes-left
   :total-number-of-bytes
   ;; Reexport binary-parser functions
   :read-value
   :write-value
   ))

(in-package :mcutiet.message)

;; Binary type definitions

(define-binary-type optional (type if length )
  (:reader (in)
           (when if (read-value type in :length length)))
  (:writer (out value)
           (when if (write-value type out value :length length))))

(define-binary-type unsigned-integer (bytes)
  (:reader (in)
           (loop with value = 0
                 for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
                   (setf (ldb (byte 8 low-bit) value) (read-byte in))
                 finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
                 do (write-byte (ldb (byte 8 low-bit) value) out))))

(define-binary-type raw-bytes (length)
  (:reader (in)
           (let ((buffer (make-array length :element-type '(unsigned-byte 8))))
             (dotimes (i length)
               (setf (aref buffer i) (read-byte in)))
             buffer))
  (:writer (out buffer)
           (dotimes (i length)
             (write-byte (aref buffer i) out))))

(define-binary-type utf8-string (length)
  (:reader (in)
           (let ((string (make-array length :element-type '(unsigned-byte 8))))
             (dotimes (i length)
               (setf (aref string i) (read-byte in)))
             (octets-to-string string)))
  (:writer (out string)
           (let ((buffer (string-to-octets string)))
             (dotimes (i length)
               (write-byte (aref buffer i) out)))))

(define-binary-type variable-length ()
  (:reader (in) (decode-size in))
  (:writer (out value)
           (let ((encoded (encode-size value)))
             (dotimes (i (length encoded))
               (write-byte (aref encoded i) out)))))

(define-binary-type u1 () (unsigned-integer :bytes 1))

(define-binary-type u2 () (unsigned-integer :bytes 2))

(define-binary-type u3 () (unsigned-integer :bytes 3))

(define-binary-type u4 () (unsigned-integer :bytes 4))

;; Symbol mapping tables

(defparameter *control-packet-types*
  #(nil
    :connect                            ; 1
    :connack                            ; 2
    :publish                            ; 3
    :puback                             ; 4
    :pubrec                             ; 5
    :pubrel                             ; 6
    :pubcomp                            ; 7
    :subscribe                          ; 8
    :suback                             ; 9
    :unsubscribe                        ; 10
    :unsuback                           ; 11
    :pingreq                            ; 12
    :pingresp                           ; 13
    :disconnect                         ; 14
    ))

(defparameter *packet-type-mapping*
  '((:connect connect)
    (:connack connack)
    (:publish publish)
    (:puback puback)
    (:pubrec pubrec)
    (:pubrel pubrel)
    (:pubcomp pubcomp)
    (:subscribe subscribe)
    (:suback suback)
    (:unsubscribe unsubscribe)
    (:unsuback unsuback)
    (:pingreq pingreq)
    (:pingresp pingresp)
    (:disconnect disconnect)))

(defparameter *qos-definitions*
  #(:at-most-once
    :at-least-once
    :exactly-once
    :reserved))

(defparameter *connect-return-codes*
  #(
    :accepted
    :refused-unacceptable-protocol-version
    :refused-identifier-rejected
    :refused-server-unavailable
    :refused-bad-username
    :refused-not-authorized
    ;; nil
    ))

;; Mapping error codes

(define-condition invalid-packet-type-error (error)
  ((raw :initarg :raw :reader raw :initform nil)
   ;; Commented out because shadows generic function `packet-type`
   ;; (packet-type :initarg :packet-type :reader packet-type :initform nil)
   ))

(define-condition invalid-qos-error (error)
  ((raw :initarg :raw :reader raw :initform nil)
   ;; Commented out because shadows generic function `qos`
   ;; (qos :initarg :qos :reader qos :initform nil)
   ))

(define-condition invalid-connect-return-error (error)
  ((raw :initarg :raw :reader raw :initform nil)
   (return-code :initarg :return-code :reader return-code :initform nil)))

;; Mapping functions

(defun qos-to-raw (value)
  :documentation "Maps qos type from symbol to int."
  (or (position value *qos-definitions*)
      (error 'invalid-qos-error :qos value)))

(defun qos-from-raw (raw)
  :documentation "Maps qos type from int to symbol."
  (when (or (>= raw (length *qos-definitions*))
            (< raw 0 ))
    (error 'invalid-qos-error :raw raw))
  (aref *qos-definitions* raw))

(defun suback-return-code-to-raw (value)
  (if (eql value :failure) 128
      (or (position value *qos-definitions*)
          (error 'invalid-qos-error :qos value))))

(defun suback-return-code-from-raw (raw)
  (cond ((= raw 128) :failure)
        ((>= raw (length *qos-definitions*)) (error 'invalid-qos-error :raw raw))
        (t (aref *qos-definitions* raw))))

(defun connect-return-code-from-raw (raw)
  :documentation "Map return code from raw to symbol."
  (when (or (>= raw (length *connect-return-codes*))
            (< raw 0))
    (error 'invalid-connect-return-error :raw raw))
  (aref *connect-return-codes* raw))

(defun connect-return-code-to-raw (value)
  :documentation "Map return code from symbol to raw value."
  (or (position value *connect-return-codes*)
      (error 'invalid-connect-return-error :return-code value)))

(defun packet-type-from-raw (raw-type)
  :documentation "Maps packet type from int to symbol."
  (when (>= raw-type (length *control-packet-types*))
    (error 'invalid-packet-type-error :raw raw-type))
  (or (aref *control-packet-types* raw-type)
      (error 'invalid-packet-type-error :raw raw-type)))

(defun packet-type-to-raw (value)
  :documentation "Maps packet type from symbol to int."
  (or (position value *control-packet-types*)
      (error 'invalid-packet-type-error :packet-type value)))

;; Functions to encode/decode remaining-length according to mqtt spec
;; 2.2.3

(defun encode-size (size)
  :documentation "Encoded packet-size SIZE with variable encoding
  scheme according to mqtt spec."
  (let ((byte 0)
        (bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer t)))
    (loop do
      (setf byte (mod size 128))
      (setf size (floor size 128))
      (when (> size 0)
        (setf byte (logior byte 128)))
      (vector-push-extend byte bytes)
          while (> size 0))
    bytes))

(define-condition packet-length-too-big (error)
  ((remaining-length :initarg :remaining-length :reader remaining-length)))

(defun decode-size (stream)
  :documentation "Decode package-size from STREAM reading one byte at
  a time and using decoding scheme from mqtt spec."
  (let ((multiplier 1)
        (encoded-byte 0)
        (value 0))
    (loop do
      (when (> multiplier (* 128 128 128))
        (error 'packet-length-too-big :remaining-length multiplier))
      (setf encoded-byte (read-byte stream))
      (setf value (+ value (* multiplier (logand encoded-byte 127))))
      (setf multiplier (* multiplier 128))
          while (/= 0 (logand encoded-byte 128)))
    value))

(defun generate-packet-identifier ()
  (random 65535))

;; Fixed header definition according to mqtt spec 2.2 and function
;; definitions to set and get fixed header flags more conveniently.

(define-tagged-binary-class
    fixed-header ()
    ((header u1)
     (remaining-length (variable-length)))
    (:dispatch (funcall #'%packet-type-to-object header)))

(defun %qos (header pos)
  :documentation "Reads qos setting from byte position pos from header."
  (qos-from-raw (ldb (byte 2 pos) header)))

(defun %dup-p (header)
  :documentation "Reads dup bit from header byte."
  (not (zerop (ldb (byte 1 3) header))))

(defun %retain-p (header)
  :documentation "Reads retain bit from header byte."
  (not (zerop (ldb (byte 1 0) header))))

(defun %packet-type (header)
  :documentation "Reads packet type from header byte"
  (packet-type-from-raw (ldb (byte 4 4) header)))

(defun %packet-type-to-object (header)
  (car (cdr (assoc (%packet-type header) *packet-type-mapping*))))

(defgeneric (setf packet-type) (value object)
  (:documentation "Writes packet type to fixed header."))

(defmethod (setf packet-type) (value (object fixed-header))
  (with-slots (header) object
    (setf (ldb (byte 4 4) header) (packet-type-to-raw value))))

(defgeneric packet-type (object)
  (:documentation "Reads packet-type from fixed header."))

(defmethod packet-type ((object fixed-header))
  (with-slots (header) object
    (%packet-type header)))

(defun %reserved-bit0-p (header)
  :documentation "Reads reserved bit 0 from header flags."
  (not (zerop (ldb (byte 1 0) header))))

(defgeneric reserved-bit0-p (fixed-header)
  (:documentation "Reads reserved bit 0 from header."))

(defmethod reserved-bit0-p ((object fixed-header))
  (with-slots (header) object
    (%reserved-bit0-p header)))

(defgeneric (setf reserved-bit0) (value fixed-header)
  (:documentation "Writes reserved bit 0 to header"))

(defmethod (setf reserved-bit0) (value (object fixed-header))
  (with-slots (header) object
    (setf (ldb (byte 1 0) header) (if  value 1 0))))

(defun %reserved-bit1-p (header)
  :documentation "Reads reserved bit 1 from header flags."
  (not (zerop (ldb (byte 1 1) header))))

(defgeneric reserved-bit1-p (fixed-header)
  (:documentation "Reads reserved bit 1 from header."))

(defmethod reserved-bit1-p ((object fixed-header))
  (with-slots (header) object
    (%reserved-bit1-p header)))

(defgeneric (setf reserved-bit1) (value fixed-header)
  (:documentation "Writes reserved-bit1 to header."))

(defmethod (setf reserved-bit1) (value (object fixed-header))
  (with-slots (header) object
    (setf (ldb (byte 1 1) header) (if value 1 0))))

(defun %reserved-bit2-p (header)
  :documentation "Reads reserved bit 2 from header flags."
  (not (zerop (ldb (byte 1 2) header))))

(defgeneric reserved-bit2-p (fixed-header)
  (:documentation "Reads reserved bit 2 from header."))

(defmethod reserved-bit2-p ((object fixed-header))
  (with-slots (header) object
    (%reserved-bit2-p header)))

(defgeneric (setf reserved-bit2) (value fixed-header)
  (:documentation "Writes reserved-bit1 to header."))

(defmethod (setf reserved-bit2) (value (object fixed-header))
  (with-slots (header) object
    (setf (ldb (byte 1 2) header) (if value 1 0))))

(defun %reserved-bit3-p (header)
  :documentation "Reads reserved bit 3 from header flags."
  (not (zerop (ldb (byte 1 3) header))))

(defgeneric reserved-bit3-p (fixed-header)
  (:documentation "Reads reserved bit 1 from header."))

(defmethod reserved-bit3-p ((object fixed-header))
  (with-slots (header) object
    (%reserved-bit3-p header)))

(defgeneric (setf reserved-bit3) (value fixed-header)
  (:documentation "Writes reserved-bit1 to header."))

(defmethod (setf reserved-bit3) (value (object fixed-header))
  (with-slots (header) object
    (setf (ldb (byte 1 3) header) (if value 1 0))))

;; Connect message definition for clients requesting a connection to a
;; server according to mqtt spec 3.1

(define-binary-class
    connect (fixed-header)
    ((protocol-name-size u2)
     (protocol-name
      (utf8-string
       :length protocol-name-size))
     (protocol-level u1)
     (connect-flags u1)
     (keep-alive u2)
     (client-id-size u2)
     (client-id
      (utf8-string
       :length client-id-size))
     (will-topic-size
      ;; will-topic-size is only set if will flag in connection flags is
      ;; set (see Mqtt 3.1.3.2)
      (optional
       :type 'u2
       :if (%will-p connect-flags)))
     (will-topic
      ;; will-topic is only set if will flag in connection flags is
      ;; set (see Mqtt 3.1.3.2)
      (optional
       :type 'utf8-string
       :length will-topic-size
       :if (%will-p connect-flags)))
     (will-message-size
      ;; will-message-size is only set if will flag in connection
      ;; flags is set (see Mqtt 3.1.3.3)
      (optional
       :type 'u2
       :if (%will-p connect-flags)))
     (will-message
      ;; will-message is only set if will flag in connection
      ;; flags is set (see Mqtt 3.1.3.3)
      (optional
       :type 'raw-bytes
       :length will-message-size
       :if (%will-p connect-flags)))
     (user-name-size
      ;; user-name-size is only set if user name flag in connection
      ;; flags is set (see Mqtt 3.1.3.4)
      (optional
       :type 'u2
       :if (%user-name-p connect-flags)))
     (user-name
      ;; user-name is only set if user name flag in connection
      ;; flags is set (see Mqtt 3.1.3.4)
      (optional
       :type 'utf8-string
       :length user-name-size
       :if (%user-name-p connect-flags)))
     (password-size
      ;; password-size is only set if password flag in connection
      ;; flags is set (see Mqtt 3.1.3.5)
      (optional
       :type 'u2
       :if (%password-p connect-flags)))
     (password
      ;; password is only set if password flag in connection flags is
      ;; set (see Mqtt 3.1.3.5). According to spec, the data are raw
      ;; bytes instead of an utf8 encoded string.
      (optional
       :type 'raw-bytes
       :length password-size
       :if (%password-p connect-flags)))))

(defun %clean-session-p (connect-flags)
  :documentation "Reads cleans session bit from connect flags."
  (logbitp 1 connect-flags))

(defun %will-p (connect-flags)
  :documentation "Reads will bit from connect flags."
  (logbitp 2 connect-flags))

(defgeneric will-flag-p (connect))

(defmethod will-flag-p ((object connect))
  (with-slots (connect-flags) object
    (%will-p connect-flags)))

(defgeneric (setf will-flag) (value connect))

(defmethod (setf will-flag) (value (object connect))
  (with-slots (connect-flags) object
    (setf (ldb (byte 1 2) connect-flags) (if value 1 0))))

(defun %will-qos (connect-flags)
  :documentation "Reads qos setting from connect flags."
  (qos-from-raw (ldb (byte 2 3) connect-flags)))

(defgeneric will-qos (connect))

(defmethod will-qos ((object connect))
  (with-slots (connect-flags) object
    (%will-qos connect-flags)))

(defgeneric (setf will-qos) (value connect))

(defmethod (setf will-qos) (value (object connect))
  (with-slots (connect-flags) object
    (setf (ldb (byte 2 3) connect-flags) (qos-to-raw value))))

(defun %will-retain-p (connect-flags)
  :documentation "Reads will-retain bit from connect flags."
  (logbitp 5 connect-flags))

(defgeneric will-retain-p (connect))

(defmethod will-retain-p ((object connect))
  (with-slots (connect-flags) object
    (%will-retain-p connect-flags)))

(defgeneric (setf will-retain) (value connect))

(defmethod (setf will-retain) (value (object connect))
  (with-slots (connect-flags) object
    (setf (ldb (byte 1 5) connect-flags) (if value 1 0))))

(defun %user-name-p (connect-flags)
  :documentation "Reads user name bit from connect flags."
  (logbitp 7 connect-flags))

(defun %password-p (connect-flags)
  :documentation "Reads password bit from connect flags."
  (logbitp 6 connect-flags))

(defgeneric user-name-p (connect))

(defmethod user-name-p ((object connect))
  (with-slots (connect-flags) object
    (%user-name-p connect-flags)))

(defgeneric password-p (connect))

(defmethod password-p ((object connect))
  (with-slots (connect-flags) object
    (%password-p connect-flags)))

(defgeneric (setf user-name-flag) (value connect))

(defmethod (setf user-name-flag) (value (object connect))
  (with-slots (connect-flags) object
    (setf (ldb (byte 1 7) connect-flags) (if value 1 0))))

(defgeneric (setf passord-flag) (value connect))

(defmethod (setf password-flag) (value (object connect))
  (with-slots (connect-flags) object
    (setf (ldb (byte 1 6) connect-flags) (if value 1 0))))

(defgeneric clean-session-p (connect)
  (:documentation "Reads clean-session bit from connect message"))

(defmethod clean-session-p ((object connect))
  (with-slots (connect-flags) object
    (%clean-session-p connect-flags)))

(defgeneric (setf clean-session) (value connect)
  (:documentation "Sets clean session bit of connect message to value."))

(defmethod (setf clean-session) (value (object connect))
  (with-slots (connect-flags) object
    (setf (ldb (byte 1 1) connect-flags) (if value 1 0))))

;; Definition of connack message for acknowledgement connection
;; request according to mqtt spec 3.2
;;
;; The connack packet is the packet sent by the server in response to
;; a connect packet received from a client. The first packet sent from
;; the server to the client must be a connack packet

(define-binary-class connack
    (fixed-header)
    ((acknowledge-flags u1)
     (return-code-flags u1)))

(defun %session-present-p (acknowledge-flags)
  :documentation "Reads session present flag"
  (logbitp 0 acknowledge-flags))

(defun %connect-return-code (return-code)
  :documentation "Reads return code from raw byte. "
  (connect-return-code-from-raw return-code))

(defgeneric session-present-p (connack)
  (:documentation "Returns true is session present bit is set."))

(defmethod session-present-p ((object connack))
  (with-slots (acknowledge-flags) object
    (%session-present-p acknowledge-flags)))

(defgeneric (setf session-present) (value connack))

(defmethod (setf session-present) (value (object connack))
  (with-slots (acknowledge-flags) object
    (setf (ldb (byte 1 0) acknowledge-flags) (if value 1 0))))

(defgeneric connect-return-code (connack)
  (:documentation "Reads the return code from connack message."))

(defmethod connect-return-code ((object connack))
  (with-slots (return-code-flags) object
    (%connect-return-code return-code-flags)))

(defgeneric (setf connect-return-code) (value object)
  (:documentation "Writes return code to connack message"))

(defmethod (setf connect-return-code) (value (object connack))
  (with-slots (return-code-flags) object
    (setf return-code-flags (connect-return-code-to-raw value))))

;; Definition of publish message for publishing messages according to
;; mqtt spec 3.3
;;
;; A publish control packet is sent from a client to a server or from
;; server to a client to transport an application message.

(define-binary-class publish
    (fixed-header)
    ((topic-name-size u2)
     (topic-name
      (utf8-string
       :length topic-name-size))
     (packet-identifier
      (optional
       :type 'u2
       ;; If qos level is greater zero (:at-most-once),
       ;; packet-identifier must be present (see Mqtt 3.3.2.2).
       :if (when (> 0 (qos-to-raw (%qos header 1))) t)))
     (payload
      (raw-bytes
       ;; The length of the payload can be calculated by subtracting
       ;; the length of the variable header from the Remaining Length
       ;; field that is in the Fixed Header (see Mqtt 3.3.3)
       :length (- remaining-length
                  ;; Length of topic-name-size and topic-name
                  (+ 2 topic-name-size)
                  ;; Packet-identifier if qos is greater 0
                  (if (> 0 (qos-to-raw (%qos header 1))) 2 0))))))

(defgeneric qos (publish)
  (:documentation "Reads qos flag from publish message."))

(defmethod qos ((object publish))
  (with-slots (header) object
    (%qos header 1)))

(defgeneric (setf qos) (value publish)
  (:documentation "Writes qos flag to publish message."))

(defmethod (setf qos) (value (object publish))
  (with-slots (header) object
    (setf (ldb (byte 2 1) header) (qos-to-raw value))))

(defgeneric dup-p (publish)
  (:documentation "Reads dup flag from publish message."))

(defmethod dup-p ((object publish))
  (with-slots (header) object
    (%dup-p header)))

(defgeneric (setf dup) (value publish)
  (:documentation "Writes dup flag to publish message."))

(defmethod (setf dup) (value (object publish))
  (with-slots (header) object
    (setf (ldb (byte 1 3) header) (if value 1 0))))

(defgeneric retain-p (publish)
  (:documentation "Reads retain flag from publish message."))

(defmethod retain-p ((object publish))
  (with-slots (header) object
    (%retain-p header)))

(defgeneric (setf retain) (value fixed-header)
  (:documentation "Writes retain flag to publish message."))

(defmethod (setf retain) (value (object publish))
  (with-slots (header) object
    (setf (ldb (byte 1 0) header) (if value 1 0))))

;; Definition of puback message for acknowledgement of publish
;; messages by broker to client according to mqtt spec 3.4.
;;
;; A puback packet is the response to a publish packet with qos level
;; 1.

(define-binary-class puback
    (fixed-header)
    ((packet-identifier u2)))

;; Definition of pubrec message (publish received) according to mqtt spec 3.5.
;;
;; A pubrec packet is the response to a publish Packet with QoS 2. It
;; is the second packet of the QoS 2 protocol exchange.

(define-binary-class pubrec
    (fixed-header)
    ((packet-identifier u2)))

;; Definition of pubrel message (publish release) according to mqtt
;; spec 3.6.
;;
;; A pubrel packet is the response to a pubrec packet. It is the third
;; packet of the QoS 2 protocol exchange.

(define-binary-class pubrel
    (fixed-header)
    ((packet-identifier u2)))

;; Definition of pubcomp message (publish complete) according to mqtt
;; spec 3.7.
;;
;; The pubcomp packet is the response to a pubrel packet. It is the
;; fourth and final packet of the QoS 2 protocol exchange.

(define-binary-class pubcomp
    (fixed-header)
    ((packet-identifier u2)))

;; Definition of subscribe message according to mqtt spec 3.8.
;;
;; The subscribe packet is sent from the client to the server to
;; create one or more subscriptions. Each subscription registers a
;; client’s interest in one or more topics. The server sends publish
;; packets to the client in order to forward application messages that
;; were published to topics that match these subscriptions. The
;; subscribe packet also specifies (for each subscription) the maximum
;; QoS with which the Server can send application messages to the
;; client.
;;
;; NOTE: Currently only one topic per subscription is supported.

(define-binary-class subscribe
    (fixed-header)
    ((packet-identifier u2)
     ;; Currently only subscribtion to one topic per subscribe
     ;; message is supported by this implementation. However mqtt
     ;; allows subscribtion requests with multiple topic filters per
     ;; subscribe message (see Mqtt 3.8.3)
     (topic-filter-size u2)
     (topic-filter
      (utf8-string
       :length topic-filter-size))
     (requested-qos-flags u1)))

(defgeneric requested-qos (subscribe)
  (:documentation "Read requested qos from subscribe message."))

(defmethod requested-qos ((object subscribe))
  (with-slots (requested-qos-flags) object
    (%qos requested-qos-flags 0)))

(defgeneric (setf requested-qos) (value subscribe)
  (:documentation "Read requested qos from subscribe message"))

(defmethod (setf requested-qos) (value (object subscribe))
  (with-slots (requested-qos-flags) object
    (setf (ldb (byte 2 0) requested-qos-flags) (qos-to-raw value))))

;; Definition of suback message (subscribe acknowledgement) according
;; to mqtt spec 3.9.
;;
;; A suback packet is sent by the server to the client to confirm
;; receipt and processing of a subscribe Packet.
;; 
;; NOTE: Sine only one subscription per subscribe message is
;; supported, only the list of return codes contains only one entry.

(define-binary-class suback
    (fixed-header)
    ((packet-identifier u2)
     (return-code-flags u1)))

(defun %failure-p (flags)
  :documentation "Returns true is failure bit (0x80) is set"
  (logbitp 7 flags))

(define-condition subscription-failure (error)
  ())

(defgeneric suback-return-code (suback)
  (:documentation "Returns the maximum qos for subscription."))

(defmethod suback-return-code ((object suback))
  (with-slots (return-code-flags) object
    (if (%failure-p return-code-flags)
        :failure
        (%qos return-code-flags 0))))

(defgeneric (setf suback-return-code) (value suback))

(defmethod (setf suback-return-code) (value (object suback))
  (with-slots (return-code-flags) object
    (setf return-code-flags (suback-return-code-to-raw value))))

;; Definition of unsubscribe message according to mqtt spec 3.10.
;;
;; An unsubscribe packet is sent by the client to the server, to
;; unsubscribe from topics.
;;
;; NOTE: Only one unsubscription from a topic per message is
;; supported.

(define-binary-class unsubscribe
    (fixed-header)
    ((packet-identifier u2)
     ;; Currently only one unsubscription topic per unsubscribe
     ;; message is supported by this implementation. However mqtt
     ;; allows unsubscribtion requests with multiple topic filters per
     ;; unsubscribe message (see Mqtt 3.10.3)
     (topic-filter-size u2)
     (topic-filter
      (utf8-string :length topic-filter-size))))

;; Definition of unsuback message (unsubscribe acknowledgement)
;; according to mqtt spec 3.11.
;;
;; The unsuback packet is sent by the server to the client to confirm
;; receipt of an unsubscribe packet.

(define-binary-class unsuback
    (fixed-header)
    ((packet-identifier u2)))

;; Definition of pingreq (ping request) message according to mqtt spec
;; 3.12.
;;
;; The pingreq packet is sent from a client to the server. It can be
;; used to:
;; 1) Indicate to the Server that the Client is alive in the absence
;;    of any other Control Packets being sent from the Client to the
;;    Server.
;; 2) Request that the Server responds to confirm that it is alive.
;; 3) Exercise the network to indicate that the Network Connection is
;;    active.

(define-binary-class pingreq
    (fixed-header)
    ())

;; Definition of pingresp (ping response) message according to mqtt
;; spec 3.13.
;;
;; A pingresp packet is sent by the server to the client in response
;; to a pingreq Packet. It indicates that the server is alive.

(define-binary-class pingresp
    (fixed-header)
    ())

;; Definition of disconnect message according to mqtt spec 3.14
;;
;; The disconnect packet is the final control packet sent from the
;; client to the server. It indicates that the client is disconnecting
;; cleanly.

(define-binary-class disconnect
    (fixed-header)
    ())

;; Methods to calculate the remaining length of each packet type.

(defgeneric calculate-remaining-length (message)
  (:documentation "Calculates the remaining length of packet `message`."))

(defmethod calculate-remaining-length ((object connect))
  (+
   ;; two bytes for protocol name size
   2
   ;; four bytes for protocol name
   4
   ;; one byte for protocol level
   1
   ;; one byte for connect flags
   1
   ;; two bytes for keep-alive time
   2
   ;; two bytes for client id size
   2
   ;; the cliend id size itself
   (client-id-size object)
   ;; if user name is present, two bytes for user name and user name
   ;; size itself
   (if (user-name-p object)
       (+ 2 (user-name-size object)) 0)
   ;; if password is present, two bytes for user name and user name
   ;; size itself
   (if (password-p object)
       (+ 2 (password-size object)) 0)
   ;; if will flag is present two bytes for will topic size and will
   ;; message size and will topic size and will message size itself
   (if (will-flag-p object)
       (+ 4
          (will-message-size object)) 0)))

(defmethod calculate-remaining-length ((object connack))
  (+
   ;; one byte for acknowledge flag.
   1
   ;; one byte for return code
   1))

(defmethod calculate-remaining-length ((object publish))
  (+
   ;; two bytes for topic filter size
   2
   ;; topic filter size itself
   (topic-name-size object)
   ;; if qos level is greater zero packet identifier must be present.
   (if (> 0 (qos-to-raw (qos object))) 2 0)
   ;; length of payload
   (length (payload object))))

(defmethod calculate-remaining-length ((object puback))
  ;; This is the length of the variable header. For the PUBACK Packet
  ;; this has the value 2. See 3.4.1
  2)

(defmethod calculate-remaining-length ((object pubrec))
  ;; This is the length of the variable header. For the PUBREC Packet
  ;; this has the value 2.
  2)

(defmethod calculate-remaining-length ((object pubrel))
  ;; This is the length of the variable header. For the PUBREL Packet
  ;; this has the value 2.
  2)

(defmethod calculate-remaining-length ((object pubcomp))
  ;; This is the length of the variable header. For the PUBCOMP Packet
  ;; this has the value 2.
  2)

(defmethod calculate-remaining-length ((object subscribe))
  (+
   ;; two bytes for packet identifier
   2
   ;; two bytes for topic filter size
   2
   ;; topic filter size itself
   (topic-filter-size object)
   ;; one byte for requested qos flag
   1))

(defmethod calculate-remaining-length ((object suback))
  (+
   ;; two bytes for the packet identifier
   2
   ;; one byte for return code
   1))

(defmethod calculate-remaining-length ((object unsubscribe))
  (+
   ;; two bytes for the packet identifier
   2
   ;; two bytes for the topic filter size
   2
   ;; topic filter size itself
   (topic-filter-size object)))

(defmethod calculate-remaining-length ((object unsuback))
  ;; This is the length of the variable header. For the unsuback Packet
  ;; this has the value 2.
  2)

(defmethod calculate-remaining-length ((object pingreq))
  ;; Remaining length is zero
  0)

(defmethod calculate-remaining-length ((object pingresp))
  ;; Remaining length is zero
  0)

(defmethod calculate-remaining-length ((object disconnect))
  ;; Remaining length is zero
  0)

(define-condition invalid-packet-configuration (error)
  ((%packet :initarg :packet :reader packet)
   (%description :initarg :description :reader description))
  (:report
   (lambda (c stream)
     (format stream "Packet ~A: ~A"
             (packet c)
             (description c)))))

(defun throw-if-invalid (packet)
  (multiple-value-bind (valid condition) (valid-packet packet)
    (when (not valid)
      (error condition)))
  t)

(defgeneric valid-reserved-bits (packet)
  (:documentation "Checks if packet has valid reserved bits"))

(defmethod valid-reserved-bits ((object fixed-header))
  ;; The remaining bits [3-0] of byte 1 in the fixed header contain
  ;; flags specific to each MQTT Control Packet type as listed in the
  ;; Table 2.2 - Flag Bits below. Where a flag bit is marked as
  ;; Reserved in Table 2.2 - Flag Bits, it is reserved for future
  ;; use and MUST be set to the value listed in that table
  ;; [MQTT-2.2.2-1]. If invalid flags are received, the receiver MUST
  ;; close the Network Connection [MQTT-2.2.2-2]. See Section 4.8 for
  ;; details about handling errors (see 2.2.2)
  (if (or (reserved-bit0-p object)
          (reserved-bit1-p object)
          (reserved-bit2-p object)
          (reserved-bit3-p object))
      nil
      t))

(defmethod valid-reserved-bits ((object pubrel))
  (if (and (or (reserved-bit0-p object)
               (reserved-bit2-p object)
               (reserved-bit3-p object))
           (not (reserved-bit1-p object)))
      nil
      t))

(defmethod valid-reserved-bits ((object subscribe))
  (if (or (or (reserved-bit0-p object)
               (reserved-bit2-p object)
               (reserved-bit3-p object))
           (not (reserved-bit1-p object)))
      nil
      t))

(defmethod valid-reserved-bits ((object unsubscribe))
  (if (or (or (reserved-bit0-p object)
              (reserved-bit2-p object)
              (reserved-bit3-p object))
          (not (reserved-bit1-p object)))
      nil
      t))

(defmethod valid-reserved-bits ((object publish))
  (if (and (reserved-bit1-p object)
           (reserved-bit2-p object))
      nil
      t))

(defgeneric valid-remaining-length (packet)
  (:documentation "Checks remaining length of packet is valid."))

(defmethod valid-remaining-length ((object fixed-header))
  (if (eql (remaining-length object)
           (calculate-remaining-length object))
      t
      nil))

(defgeneric valid-packet (packet)
  (:documentation "Checks if packet is valid according to the mqtt
  spec"))

(defmethod valid-packet ((object fixed-header))

  (unless (valid-reserved-bits object)
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet (packet-type object)
        :description "Configuration of reserved bits of fixed header violated."))))

  (unless (valid-remaining-length object)
    (return-from valid-packet
      (values
       nil
       (make-condition 'invalid-packet-configuration
                       :packet (packet-type object)
                       :description "Remaining length is invalid "))))
  (values t nil))

(defmethod valid-packet ((object connect))

  (unless (client-id object)
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'connect
        :description "Required client-id not set."))))

  (unless (string= (protocol-name object) "MQTT")
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'connect
        :description "Invalid protocol name."))))

  ;; if the Will Flag is set to 0 the Will QoS and Will Retain fields in
  ;; the Connect Flags MUST be set to zero and the Will Topic and Will
  ;; Message fields MUST NOT be present in the payload (see 3.1.2.5)
  (unless (will-flag-p object)
    (when (or
           (not (eql (will-qos object) :at-most-once))
           (will-retain-p object)
           (will-topic object)
           (will-message object))
      (return-from valid-packet
        (values
         nil
         (make-condition 'invalid-packet-configuration
                         :packet 'connect
                         :description "Configuration of will-flags violated.")))))

  ;; If the Will Flag is set to 1, the Will QoS and Will Retain fields
  ;; in the Connect Flags will be used by the Server, and the Will Topic
  ;; and Will Message fields MUST be present in the payload (see 3.1.2.5)
  (when (will-flag-p object)
    (when (or
           (not (will-topic object))
           (not (will-message object)))
      (return-from valid-packet
        (values
         nil
         (make-condition
          'invalid-packet-configuration
          :packet 'connect
          :description "Configuration of will-flags violated.")))))

  ;; If the User Name Flag is set to 0, a user name MUST NOT be present
  ;; in the payload [MQTT-3.1.2-18].
  ;; If the User Name Flag is set to 1, a user name MUST be present in the
  ;; payload [MQTT-3.1.2-19].
  (when (or (and (not (user-name-p object)) (user-name object))
            (and (user-name-p object) (not (user-name object))))
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'connect
        :description "Configuration of username flags violated."))))

  ;; If the Password Flag is set to 1, a password MUST be present in
  ;; the payload [MQTT-3.1.2-21].  If the User Name Flag is set to 0,
  ;; the Password Flag MUST be set to 0 [MQTT-3.1.2-22].
  (when (or (and (not (password-p object)) (password object))
            (and (password-p object) (not (password object))))
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'connect
        :description "Configuration of password flags violated."))))

  ;; If the User Name Flag is set to 0, the Password Flag MUST be set to 0
  (when (and (not (user-name-p object)) (password-p object))
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'connect
        :description "Password flag set but not username flag."))))

  (call-next-method))

(defmethod valid-packet ((object connack))
  (call-next-method))

(defmethod valid-packet ((object publish))

  ;; The Topic Name MUST be present as the first field in the PUBLISH
  ;; Packet Variable header. It MUST be a UTF-8 encoded string
  (unless (topic-name object)
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'publish
        :description "Requireded topic-name not set"))))

  ;; A PUBLISH Packet MUST NOT have both QoS bits set to 1. If a
  ;; Server or Client receives a PUBLISH Packet which has both QoS
  ;; bits set to 1 it MUST close the Network Connection (see 3.3.1.2)
  (when (eql (qos object) :reserved)
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'publish
        :description "Qos must not be :reserved"))))

  ;; A PUBLISH Packet MUST NOT contain a Packet Identifier if its QoS value is set to 0
  (when (and (= 0 (qos-to-raw (qos object)))
             (packet-identifier object))
    (return-from valid-packet
      (values
       nil
       (make-condition
        'invalid-packet-configuration
        :packet 'publish
        :description "Must not contain a packet identifier is qos is set to 0 "))))

  (call-next-method))


(defmethod initialize-instance :after ((object fixed-header) &key)
  ;; Initialize header and remaining length with 0
  (setf (header object) 0)
  (setf (remaining-length object) 0))

;; TODO: Unit test
;; (make-instance 'mcutiet.message:connect :will-flag nil :will-topic nil :will-message nil :will-retain t)
;; (make-instance 'mcutiet.message:connect :will-flag t :will-topic "df" :will-message "ter" :will-retain nil :will-qos :at-most-once)

(defmethod initialize-instance :after  ((object connect)  &key
                                                            (clean-session t)
                                                            (will-flag nil)
                                                            (will-qos :at-most-once)
                                                            (will-retain nil)
                                                            (password-flag nil)
                                                            (user-name-flag nil)
                                                            (keep-alive 0)
                                                            client-id
                                                            (username nil)
                                                            (password nil)
                                                            (will-topic nil) 
                                                            (will-message nil))

  (unless client-id
    (error 'invalid-packet-configuration
           :packet :connect
           :description "Required client-id not set."))
  
  ;; Set bit0-3 of fixed header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  ;; Set packet-type to :connect
  (setf (packet-type object) :connect)

  ;; Start initializing varibale header
  ;; Initialize according to Mqtt spec 3.1.2.1
  (setf (protocol-name-size object) 4)
  (setf (protocol-name object) "MQTT")
  (setf (protocol-level object) 4)
  (setf (connect-flags object) 0)

  ;; Set clean-session (see Mqtt spec 3.1.2.4)
  (setf (clean-session object) clean-session)

  ;; Set user and password flags
  (setf (user-name-flag object) user-name-flag)
  (setf (password-flag object) password-flag)

  (setf (user-name object) username)
  (setf (password object) password)

  ;; If username or password was provided, we will automatically
  ;; calculate the necessary length and set *-size approprietly.
  (if username
      (setf (user-name-size object) (length username))
      (setf (user-name-size object) nil))
  
  (if password
      (setf (password-size object) (length password))
      (setf (password-size object) nil))

  ;; Set will related flags
  (setf (will-flag object) will-flag)
  (setf (will-qos object) will-qos)
  (setf (will-retain object) will-retain)

  ;; Set will message and topic
  (setf (will-topic object) will-topic)
  (setf (will-message object) will-message)
  
  (if will-topic
      (setf (will-topic-size object) (length will-topic))
      (setf (will-topic-size object) nil))

  (if will-message
      (setf (will-message-size object) (length will-message))
      (setf (will-message-size object) nil))

  (setf (keep-alive object) keep-alive)
  (setf (client-id object) client-id)

  (if client-id
      (setf (client-id-size object) (length client-id))
      (setf (client-id-size object) nil))

  ;; Set remaining length
  (setf (remaining-length object) (calculate-remaining-length object))

  object)


(defmethod initialize-instance :after ((object connack) &key
                                                          session-present
                                                          return-code)

  (unless session-present
    (error 'invalid-packet-configuration
           :packet :connack
           :description "Required session-present field not set."))

  (unless return-code
    (error 'invalid-packet-configuration
           :packet :connack
           :description "Required return-code not set."))


  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :connack)

  (setf (acknowledge-flags object) 0)
  (setf (return-code-flags object) 0)
  (setf (session-present object) session-present)
  (setf (connect-return-code object) return-code)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object publish) &key
                                                        topic-name
                                                        (payload nil)
                                                        (packet-identifier nil)
                                                        (retain nil)
                                                        (qos :at-most-once)
                                                        (dup nil))

  ;; The Topic Name MUST be present as the first field in the PUBLISH
  ;; Packet Variable header. It MUST be a UTF-8 encoded string
  (unless topic-name
    (error 'invalid-packet-configuration
           :packet :publish
           :description "Required topic-name not set"))
 
  (setf (packet-type object) :publish)
  (setf (qos object) qos)
  (setf (retain object) retain)
  (setf (dup object) dup)
  (setf (packet-identifier object) packet-identifier)
  (setf (topic-name-size object) (length topic-name))
  (setf (topic-name object) topic-name)
  (setf (payload object) payload)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object puback) &key
                                                         packet-identifier)
  
  (unless packet-identifier
    (error 'invalid-packet-configuration
           :packet :puback
           :description "Required packet-identifier not set."))
  
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :puback)
  (setf (packet-identifier object) packet-identifier)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)


(defmethod initialize-instance :after ((object pubrel) &key packet-identifier)

  (unless packet-identifier
    (error 'invalid-packet-configuration
           :packet :pubrel
           :description "Required packet-identifier not set."))
  
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) t)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :pubrel)
  (setf (packet-identifier object) packet-identifier)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object pubcomp) &key packet-identifier)

  (unless packet-identifier
    (error 'invalid-packet-configuration
           :packet :pubcomp
           :description "Required packet-identifier not set."))
  
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :pubcomp)
  (setf (packet-identifier object) packet-identifier)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object subscribe) &key
                                                            packet-identifier
                                                            topic-filter
                                                            requested-qos)
  (unless packet-identifier
    (error 'invalid-packet-configuration
           :packet :subscribe
           :description "Required packet-identifier not set."))

  (unless topic-filter
    (error 'invalid-packet-configuration
           :packet :subscribe
           :description "Required topic-filter not set."))
  
  (unless requested-qos
    (error 'invalid-packet-configuration
           :packet :subscribe
           :description "Required requested-qos not set."))
  
  
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) t)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :subscribe)
  (setf (requested-qos-flags object) 0)
  (setf (packet-identifier object) packet-identifier)
  (setf (topic-filter-size object) (length topic-filter))
  (setf (topic-filter object) topic-filter)
  (setf (requested-qos object) requested-qos)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)


(defmethod initialize-instance :after ((object suback) &key
                                                         packet-identifier
                                                         return-code)

  (unless packet-identifier
    (error 'invalid-packet-configuration
           :packet :suback
           :description "Required packet-identifier not set."))

  (unless return-code
    (error 'invalid-packet-configuration
           :packet :suback
           :description "Required packet-identifier not set."))

  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :suback)
  (setf (packet-identifier object) packet-identifier)
  (setf (return-code-flags object) (suback-return-code-to-raw return-code))
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object unsubscribe) &key
                                                              packet-identifier
                                                              topic-filter)
  (unless packet-identifier
    (error 'invalid-packet-configuration
           :packet :unsubscribe
           :description "Required packet-identifier not set."))

    (unless topic-filter
      (error 'invalid-packet-configuration
             :packet :unsubscribe
             :description "Required topic-filter not set."))

  
  
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) t)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :unsubscribe)
  (setf (packet-identifier object) packet-identifier)
  (setf (topic-filter-size object) (length topic-filter))
  (setf (topic-filter object) topic-filter)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object unsuback) &key packet-identifier)

  (unless packet-identifier
    (error 'invalid-packet-configuration
           :packet :unsuback
           :description "Required packet-identifier not set."))
  
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)

  (setf (packet-type object) :unsuback)
  (setf (packet-identifier object) packet-identifier)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object pingreq) &key)
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)
  (setf (packet-type object) :pingreq)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object pingresp) &key)
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)
  (setf (packet-type object) :pingresp)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defmethod initialize-instance :after ((object disconnect) &key)
  ;; Set bit0-3 of fixe-header according to Mqtt spec 2.2.2 Explicitly
  ;; done here for documentation purposes. Header is already
  ;; initialized with 0 in less specific `make-message` invocation.
  (setf (reserved-bit0 object) nil)
  (setf (reserved-bit1 object) nil)
  (setf (reserved-bit2 object) nil)
  (setf (reserved-bit3 object) nil)
  (setf (packet-type object) :disconnect)
  (setf (remaining-length object) (calculate-remaining-length object))
  object)

(defclass remaining-length ()
  ((multiplier :initform 1)
   (encoded-byte :initform 0)
   (value :initform 0 :reader total-number-of-bytes)
   (bytes-left :initform nil :accessor bytes-left)))

(defclass incoming-message ()
  ((remaining-length :initform (make-instance 'remaining-length) :accessor remaining-length)
   (bytes :initform (make-array 0 :adjustable t :element-type '(unsigned-byte 8) :fill-pointer 0) :reader bytes)))

(defgeneric decode-size-no-hang (remaining-length stream))

(defmethod decode-size-no-hang ((object remaining-length) stream)
  (with-slots (encoded-byte value multiplier bytes-left) object
    (when bytes-left
      (return-from decode-size-no-hang bytes-left))
    (loop
      when (> multiplier (* 128 128 128))
        do
           (error 'packet-length-too-big :remaining-length multiplier)
      unless (listen stream) do (return-from decode-size-no-hang nil)
        do
           (setf encoded-byte (read-byte stream))
           (setf value (+ value (* multiplier (logand encoded-byte 127))))
           (setf multiplier (* multiplier 128))
      while
      (/= 0 (logand encoded-byte 128)))
    (unless bytes-left
      (setf bytes-left value ))
    t))


;;
;; Sample serialization and deseralization
;;
;; (defparameter *connect* #P"connect_cmd.bin")
;; (defparameter *con* (read-message-from-file *connect*))
;; (defparameter *message* (make-message
;;                          (make-instance 'connect)
;;                          :username "hello"
;;                          :password (babel:string-to-octets "dfdfdf")
;;                          :will-topic "dfdf/df"
;;                          :will-message #()
;;                          :will-qos :at-most-once))

;; (write-message-to-file #p "test.bin" *message*)
;; (read-message-from-file #p "test.bin")

;; (defparameter *connect* #P"./mqtt-client/data/connect_cmd.bin")
;; (defparameter *connack* #P"./mqtt-client/data/connect_ack.bin")
;; (defparameter *disconnect* #P"./mqtt-client/data/disconnect_req.bin")
;; (defparameter *publish* #P"./mqtt-client/data/publish_cmd.bin")
;; (defparameter *suback* #P "./mqtt-client/data/subscribe_ack.bin")
;; (defparameter *subscribe* #P "./mqtt-client/data/subscribe_req.bin")

;; (read-message-from-file *connect*)
;; (read-message-from-file *connack*)
;; (read-message-from-file *publish*)
;; (read-message-from-file *disconnect*)
;; (read-message-from-file *subscribe*)
;; (read-message-from-file *suback*)

;; Test message-to-octets / octets-to-message
;; (octets-to-message (message-to-octets (make-connect "dfdf" t 300)))


;; (let* ((header 3)
;;        (remaining-length 3))
;;   (let ((object
;;           (allocate-instance
;;            (find-class 'fixed-header))))
;;     (setf (slot-value object 'header) header)
;;     (setf (slot-value object 'remaining-length) remaining-length)
;;     ;; (with-slots (header remaining-length)
;;     ;;     object
;;     ;;   (setf header 0)
;;     ;;   (setf remaining-length 0))
    
;;     object))
