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

(5am:in-suite streams-tests)

(def-fixture input-stream-fixture (data)
  (let ((stream (make-instance 'mcutiet.streams:vector-input-stream :data data)))
    (&body)))

(test test-vector-input-stream
  "Test class vector-input-stream"

  ;; Test read on initialized stream
  (with-fixture input-stream-fixture (#(1 2 3))
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-read-byte stream) 1))
    (is (eql (mcutiet.streams::stream-read-byte stream) 2))
    (is (eql (mcutiet.streams::stream-read-byte stream) 3))
    (is (eql (mcutiet.streams::stream-read-byte stream) :eof))
    (is (eql (mcutiet.streams:stream-position stream) 3)))
  ;; Test stream-position resettting
  (with-fixture input-stream-fixture (#(1 2 3))
    (setf (mcutiet.streams:stream-position stream) 2)
    (is (eql (mcutiet.streams:stream-position stream) 2))
    (is (eql (mcutiet.streams::stream-read-byte stream) 3))
    (setf (mcutiet.streams:stream-position stream) 0)
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-read-byte stream) 1))
    (setf (mcutiet.streams:stream-position stream) 3)
    (is (eql (mcutiet.streams::stream-read-byte stream) :eof))
    (signals error (setf (mcutiet.streams:stream-position stream) 4))
    (signals error (setf (mcutiet.streams:stream-position stream) -1)))
  ;; Test empty stream
  (with-fixture input-stream-fixture (nil)
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-read-byte stream) :eof))
    (is (eql (mcutiet.streams:stream-position stream) 0))))

(def-fixture input-stream-displaced-fixture (data)
  (let ((stream (make-instance 'mcutiet.streams:vector-input-stream :data data :displaced t)))
    (&body)))

(test test-vector-input-displaced-stream
  "Test class vector-input-stream-displaced"

  ;; Test read on initialized stream
  (with-fixture input-stream-displaced-fixture
      ((make-array 3 :element-type '(unsigned-byte 8)
                     :initial-contents #(1 2 3)))
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-read-byte stream) 1))
    (is (eql (mcutiet.streams::stream-read-byte stream) 2))
    (is (eql (mcutiet.streams::stream-read-byte stream) 3))
    (is (eql (mcutiet.streams::stream-read-byte stream) :eof))
    (is (eql (mcutiet.streams:stream-position stream) 3)))
  ;; Test stream-position resettting
  (with-fixture input-stream-displaced-fixture
      ((make-array 3 :element-type '(unsigned-byte 8)
                     :initial-contents #(1 2 3)))
    (setf (mcutiet.streams:stream-position stream) 2)
    (is (eql (mcutiet.streams:stream-position stream) 2))
    (is (eql (mcutiet.streams::stream-read-byte stream) 3))
    (setf (mcutiet.streams:stream-position stream) 0)
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-read-byte stream) 1))
    (setf (mcutiet.streams:stream-position stream) 3)
    (is (eql (mcutiet.streams::stream-read-byte stream) :eof))
    (signals error (setf (mcutiet.streams:stream-position stream) 4))
    (signals error (setf (mcutiet.streams:stream-position stream) -1)))
  ;; Test empty stream
  (with-fixture input-stream-displaced-fixture
      ((make-array 0 :element-type '(unsigned-byte 8)))
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-read-byte stream) :eof))
    (is (eql (mcutiet.streams:stream-position stream) 0))))


(def-fixture output-stream-fixture (data)
  (let ((stream (make-instance 'mcutiet.streams:vector-output-stream :data data )))
    (&body)))

(test test-vector-output-stream
  "Test method `stream-read-byte`"
  ;; Write to empty stream
  (with-fixture output-stream-fixture (nil)
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-write-byte stream 1) 1))
    (is (equalp (mcutiet.streams::data stream) #(1)))
    (is (eql (mcutiet.streams:stream-position stream) 1)))
  ;; Write to initialized stream without resetting position (override)
  (with-fixture output-stream-fixture (#(1 2 3))
    (is (eql (mcutiet.streams:stream-position stream) 0))
    (is (eql (mcutiet.streams::stream-write-byte stream 6) 6))
    (is (equalp (mcutiet.streams::data stream) #(6 2 3)))
    (is (eql (mcutiet.streams:stream-position stream) 1)))
  ;; Write to initialized stream with resetting position
  (with-fixture output-stream-fixture (#(1 2 3))
    (setf (mcutiet.streams:stream-position stream) 3)
    (is (eql (mcutiet.streams:stream-position stream) 3))
    (is (eql (mcutiet.streams::stream-write-byte stream 6) 6))
    (is (equalp (mcutiet.streams::data stream) #(1 2 3 6)))
    (is (eql (mcutiet.streams:stream-position stream) 4))))
