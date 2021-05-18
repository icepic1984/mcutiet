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

(in-package :mcutiet.tests)

(5am:in-suite io-tests)

(def-fixture message-stream ()
  (let ((stream (mcutiet.io:message-to-input-stream
                 (make-instance 'connect :client-id "bla"))))
    (&body)))

(test read-raw-message-no-hang
  "Test function `read-raw-message-no-hang`"
  (with-fixture message-stream ()
    (let ((rl (make-instance 'incoming-message)))
      (is (mcutiet.io:read-raw-message-no-hang stream rl))
      (is (equalp (data stream) (bytes rl)))))
  (with-fixture message-stream ()
    (let ((rl (make-instance 'incoming-message)))
      (setf (fill-pointer (data stream)) 0)
      (loop
        with i = 0
        while (not (mcutiet.io:read-raw-message-no-hang stream rl))
        do
           (setf (fill-pointer (data stream)) i)
           (incf i))
      (is (equalp (data stream) (bytes rl))))))

(test read-raw-message
  "Test function `read-raw-message`"
  (let ((message (mcutiet.io:message-to-input-stream
                  (make-instance 'connect :client-id "bla"))))
    (is (equalp (mcutiet.io:read-raw-message message) (data message)))))

(test message-to-octets
  "Test function `message-to-octets`"
  (let ((bytes #(16 15 0 4 77 81 84 84 4 2 0 0 0 3 98 108 97))
        (message (make-instance 'connect :client-id "bla")))
    (is (equalp (mcutiet.io:message-to-octets message) bytes))))

(test octets-to-message
  "Test function `octets-to-message`"
  (let ((message (mcutiet.io:message-to-input-stream (make-instance 'connect :client-id "bla"))))
    (is (equalp
         (mcutiet.io:message-to-octets
          (mcutiet.io:octets-to-message (data message))) (data message)))))

(def-fixture array-stream-fixture (content)
  (let ((stream (make-instance 'vector-input-stream :data content))
        (array (make-array (length content)
                           :element-type '(unsigned-byte 8))))
    (&body)))

(test read-sequence-no-hang
  "Test function `read-sequence-no-hang`"
  (with-fixture array-stream-fixture (#(1 2 3 4 5))
    (is (eql (length (data stream)) (mcutiet.io:read-sequence-no-hang array stream)))
    (is (equalp (data stream) array)))
  (with-fixture array-stream-fixture (#(1 2 3 4 5))
    (is (= 2 (mcutiet.io:read-sequence-no-hang array stream :start 2 :end 4)))
    (is (equalp #(0 0 1 2 0) array)))
  (with-fixture array-stream-fixture (#(1 2 3 4 5))
    (loop for i from 1 to (length (data stream))
          do
             (setf (fill-pointer (data stream)) i)
             (is (= 1  (mcutiet.io:read-sequence-no-hang array stream :start (1- i)))))
    (setf (stream-position stream) 0)
    (is (equalp (data stream) array))))


(test vector-push-extend-seq
  "Test function `vector-push-extend-seq`"
  (let ((array (make-array 3 :initial-contents #(1 2 3) :adjustable t :fill-pointer 3))
        (src (make-array 3 :initial-contents #(4 5 6))))
    (mcutiet.io:vector-push-extend-seq array src)
    (is (equalp array #(1 2 3 4 5 6)))))



