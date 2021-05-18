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
(defpackage :mcutiet.streams
  (:use :cl :trivial-gray-streams)
  (:export
   :vector-input-stream
   :vector-output-stream
   :vector-bidirectional-stream
   :stream-position
   :setf stream-position
   :data))

(in-package :mcutiet.streams)

(deftype octet () '(unsigned-byte 8))

(defclass vector-base-stream ()
  ((data :initarg :data :type (vector octet) :reader data)
   (stream-position :initarg :stream-position :initform 0 :reader stream-position)))


(defgeneric (setf stream-position) (value object)
  (:documentation "Resets the stream position to value."))

(defmethod (setf stream-position) (value (object vector-base-stream))
  (when (or  (> value (length (data object)))
             (< value 0))
    (error "Stream position out of bound."))
  (setf (slot-value object 'stream-position) value))

(defclass vector-input-stream
    (vector-base-stream fundamental-binary-input-stream)
  ())

(defmethod stream-read-byte ((stream vector-input-stream))
  (with-slots (data stream-position) stream
    (if (< stream-position (length data))
        (prog1 (aref data stream-position)
          (incf stream-position))
        :eof)))

(defmethod stream-listen ((stream vector-input-stream))
  (with-slots (data stream-position) stream
    (if (< stream-position (length data))
        t
        nil)))

(defmethod stream-element-type ((stream vector-base-stream))
  '(unsigned-byte 8))

(defclass vector-output-stream
    (vector-base-stream fundamental-binary-output-stream)
  ())

(defmethod stream-write-byte ((stream vector-output-stream) value)
  (with-slots (data stream-position) stream
    (if (< stream-position (length data))
        (prog1 (setf (aref data stream-position) value)
          (incf stream-position))
        (prog1 (vector-push-extend value data)
          (incf stream-position))))
  value)

(defclass vector-bidirectional-stream
    (vector-output-stream
     vector-input-stream)
  ())

(defmethod initialize-instance :after ((stream vector-output-stream) &key data displaced)
  (setf (slot-value stream 'data)
        (if (not displaced)
            (make-array (length data)
                        :adjustable t
                        :element-type 'octet
                        :fill-pointer (length data)
                        :initial-contents data)
            (make-array (length data)
                        :adjustable t
                        :element-type 'octet
                        :fill-pointer (length data)
                        :displaced-to data))))

(defmethod initialize-instance :after ((stream vector-input-stream) &key data displaced)
  (setf (slot-value stream 'data)
        (if (not displaced)
            (make-array (length data)
                        :adjustable t
                        :element-type 'octet
                        :fill-pointer (length data)
                        :initial-contents data)
            (make-array (length data)
                        :adjustable t
                        :element-type 'octet
                        :fill-pointer (length data)
                        :displaced-to data))))


(defmethod initialize-instance :after ((stream vector-bidirectional-stream) &key data displaced)
  (setf (slot-value stream 'data)
        (if (not displaced)
            (make-array (length data)
                        :adjustable t
                        :element-type 'octet
                        :fill-pointer (length data)
                        :initial-contents data)
            (make-array (length data)
                        :adjustable t
                        :element-type 'octet
                        :fill-pointer (length data)
                        :displaced-to data))))





