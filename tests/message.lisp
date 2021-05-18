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

(5am:in-suite message-tests-internal)

(test packet-type-to-raw-test
  "Test conversion from packet type to raw data"
  (is (= (mcutiet.message::packet-type-to-raw :connect) 1))
  (is (= (mcutiet.message::packet-type-to-raw :connack) 2))
  (is (= (mcutiet.message::packet-type-to-raw :publish) 3))
  (is (= (mcutiet.message::packet-type-to-raw :puback) 4))
  (is (= (mcutiet.message::packet-type-to-raw :pubrec) 5))
  (is (= (mcutiet.message::packet-type-to-raw :pubrel) 6))
  (is (= (mcutiet.message::packet-type-to-raw :pubcomp) 7))
  (is (= (mcutiet.message::packet-type-to-raw :subscribe) 8))
  (is (= (mcutiet.message::packet-type-to-raw :suback) 9))
  (is (= (mcutiet.message::packet-type-to-raw :unsubscribe) 10))
  (is (= (mcutiet.message::packet-type-to-raw :unsuback) 11))
  (is (= (mcutiet.message::packet-type-to-raw :pingreq) 12))
  (is (= (mcutiet.message::packet-type-to-raw :pingresp) 13))
  (is (= (mcutiet.message::packet-type-to-raw :disconnect) 14))
  (signals mcutiet.message::invalid-packet-type-error
      (mcutiet.message::packet-type-to-raw :fail)))

(test packet-type-from-raw-test
  "Test conversion from raw data to packet type"
  (is (eql (mcutiet.message::packet-type-from-raw 1) :connect))
  (is (eql (mcutiet.message::packet-type-from-raw 2) :connack))
  (is (eql (mcutiet.message::packet-type-from-raw 3) :publish))
  (is (eql (mcutiet.message::packet-type-from-raw 4) :puback))
  (is (eql (mcutiet.message::packet-type-from-raw 5) :pubrec))
  (is (eql (mcutiet.message::packet-type-from-raw 6) :pubrel))
  (is (eql (mcutiet.message::packet-type-from-raw 7) :pubcomp))
  (is (eql (mcutiet.message::packet-type-from-raw 8) :subscribe))
  (is (eql (mcutiet.message::packet-type-from-raw 9) :suback))
  (is (eql (mcutiet.message::packet-type-from-raw 10) :unsubscribe))
  (is (eql (mcutiet.message::packet-type-from-raw 11) :unsuback))
  (is (eql (mcutiet.message::packet-type-from-raw 12) :pingreq))
  (is (eql (mcutiet.message::packet-type-from-raw 13) :pingresp))
  (is (eql (mcutiet.message::packet-type-from-raw 14) :disconnect))
  (signals mcutiet.message::invalid-packet-type-error
      (mcutiet.message::packet-type-from-raw 0))
  (signals mcutiet.message::invalid-packet-type-error
      (mcutiet.message::packet-type-from-raw 15)))

(test qos-to-raw
  "Test conversion from will qos to raw data"
  (is (= (mcutiet.message::qos-to-raw :at-most-once) 0))
  (is (= (mcutiet.message::qos-to-raw :at-least-once) 1))
  (is (= (mcutiet.message::qos-to-raw :exactly-once) 2))
  (is (= (mcutiet.message::qos-to-raw :reserved) 3))
  (signals mcutiet.message::invalid-qos-error
      (mcutiet.message::qos-to-raw :fail)))

(test qos-from-raw
  "Test conversion from raw data to packet type"
  (is (eql (mcutiet.message::qos-from-raw 0) :at-most-once))
  (is (eql (mcutiet.message::qos-from-raw 1) :at-least-once))
  (is (eql (mcutiet.message::qos-from-raw 2) :exactly-once))
  (is (eql (mcutiet.message::qos-from-raw 3) :reserved))
  (signals mcutiet.message::invalid-qos-error
      (mcutiet.message::qos-from-raw -1))
  (signals mcutiet.message::invalid-qos-error
      (mcutiet.message::qos-from-raw 4)))

(test suback-return-code-to-raw
  "Test conversion from suback return code to raw data"
  (is (= (mcutiet.message::suback-return-code-to-raw :at-most-once) 0))
  (is (= (mcutiet.message::suback-return-code-to-raw :at-least-once) 1))
  (is (= (mcutiet.message::suback-return-code-to-raw :exactly-once) 2))
  (is (= (mcutiet.message::suback-return-code-to-raw :reserved) 3))
  (is (= (mcutiet.message::suback-return-code-to-raw :failure) 128))
  (signals mcutiet.message::invalid-qos-error
      (mcutiet.message::qos-to-raw :fail)))

(test suback-return-code-from-raw
  "Test conversion from raw data to suback return code"
  (is (eql (mcutiet.message::suback-return-code-from-raw 0) :at-most-once))
  (is (eql (mcutiet.message::suback-return-code-from-raw 1) :at-least-once))
  (is (eql (mcutiet.message::suback-return-code-from-raw 2) :exactly-once))
  (is (eql (mcutiet.message::suback-return-code-from-raw 3) :reserved))
  (is (eql (mcutiet.message::suback-return-code-from-raw 128) :failure))
  (signals mcutiet.message::invalid-qos-error
      (mcutiet.message::qos-from-raw -1))
  (signals mcutiet.message::invalid-qos-error
      (mcutiet.message::qos-from-raw 4)))

(test connect-return-code-to-raw
  "Test conversion from connect return code to raw data"
  (is (= (mcutiet.message::connect-return-code-to-raw :accepted) 0))
  (is (= (mcutiet.message::connect-return-code-to-raw :refused-unacceptable-protocol-version) 1))
  (is (= (mcutiet.message::connect-return-code-to-raw :refused-identifier-rejected) 2))
  (is (= (mcutiet.message::connect-return-code-to-raw :refused-server-unavailable) 3))
  (is (= (mcutiet.message::connect-return-code-to-raw :refused-bad-username) 4))
  (is (= (mcutiet.message::connect-return-code-to-raw :refused-not-authorized) 5))
  (signals mcutiet.message::invalid-connect-return-error
      (mcutiet.message::connect-return-code-to-raw :fail)))

(test connect-return-code-from-raw
  "Test conversion from raw data to connect return code"
  (is (eql (mcutiet.message::connect-return-code-from-raw 0) :accepted))
  (is (eql (mcutiet.message::connect-return-code-from-raw 1) :refused-unacceptable-protocol-version))
  (is (eql (mcutiet.message::connect-return-code-from-raw 2) :refused-identifier-rejected))
  (is (eql (mcutiet.message::connect-return-code-from-raw 3) :refused-server-unavailable))
  (is (eql (mcutiet.message::connect-return-code-from-raw 4) :refused-bad-username))
  (is (eql (mcutiet.message::connect-return-code-from-raw 5) :refused-not-authorized))
  (signals mcutiet.message::invalid-connect-return-error
    (mcutiet.message::connect-return-code-from-raw -1))
  (signals mcutiet.message::invalid-connect-return-error
    (mcutiet.message::connect-return-code-from-raw 6)))

(test %qos
  "Test function `qos`"
  (is (eql (mcutiet.message::%qos 0 1) :at-most-once)
      "Read :at-most-once")
  (is (eql (mcutiet.message::%qos 2 1) :at-least-once)
      "Read :at-least-once")
  (is (eql (mcutiet.message::%qos 4 1) :exactly-once)
      "Read :exactly-once")
  (is (eql (mcutiet.message::%qos 6 1) :reserved)
      "Read :reserved"))

(test %dup-p
  "Test function %dup-p"
  (is (eql (mcutiet.message::%dup-p 8) t))
  (is (eql (mcutiet.message::%dup-p 0) nil)))

(test %retain-p
  "Test function `%retain-p`"
  (is (eql (mcutiet.message::%retain-p #b0001) t))
  (is (eql (mcutiet.message::%retain-p #b0000) nil)))

(test %will-p
  "Test function `%will-p`"
  (is (eql (mcutiet.message::%will-p #b100) t))
  (is (eql (mcutiet.message::%will-p 0) nil)))

(test %packet-type
  "Test function `%packet-type`"
  (is (eql (mcutiet.message::%packet-type 16) :connect))
  (is (eql (mcutiet.message::%packet-type 224) :disconnect)))

(test %reserved-bitn
  "Test functions %reserved-bitn"
  (is (eql (mcutiet.message::%reserved-bit0-p 0) nil))
  (is (eql (mcutiet.message::%reserved-bit0-p 1) t))
  (is (eql (mcutiet.message::%reserved-bit1-p 0) nil))
  (is (eql (mcutiet.message::%reserved-bit1-p 2) t))
  (is (eql (mcutiet.message::%reserved-bit2-p 0) nil))
  (is (eql (mcutiet.message::%reserved-bit2-p 4) t))
  (is (eql (mcutiet.message::%reserved-bit3-p 0) nil))
  (is (eql (mcutiet.message::%reserved-bit3-p 8) t)))

(test %clean-session-p
  "Test function %clean-session-p"
  (is (eql (mcutiet.message::%clean-session-p 0) nil))
  (is (eql (mcutiet.message::%clean-session-p 2) t)))

(test %user-name-p
  "Test function %user-name-p"
  (is (eql (mcutiet.message::%user-name-p 0) nil))
  (is (eql (mcutiet.message::%user-name-p 128) t)))

(test %password-p
  "Test function %password-p"
  (is (eql (mcutiet.message::%password-p 0) nil))
  (is (eql (mcutiet.message::%password-p 64) t)))

(test %session-present-p
  "Test function %session-present-p"
  (is (eql (mcutiet.message::%session-present-p 0) nil))
  (is (eql (mcutiet.message::%session-present-p 1) t)))

(test %failure-p
  "Test function %failure-p"
  (is (eql (mcutiet.message::%failure-p 0) nil))
  (is (eql (mcutiet.message::%failure-p 128) t)))

(def-fixture fixed-header-fixture (header)
  (let ((object (make-instance 'mcutiet.message::fixed-header)))
    (with-slots (mcutiet.message::header) object
      (setf mcutiet.message::header header))
    (&body)))

(test reserved-bitn
  "Test methods `reserved-bitn`"
  (with-fixture fixed-header-fixture (0)
    (is (eql (mcutiet.message::reserved-bit0-p object) nil)))
  (with-fixture fixed-header-fixture (1)
    (is (eql (mcutiet.message::reserved-bit0-p object) t)))
  (with-fixture fixed-header-fixture (0)
    (is (eql (mcutiet.message::reserved-bit1-p object) nil)))
  (with-fixture fixed-header-fixture (2)
    (is (eql (mcutiet.message::reserved-bit1-p object) t)))
  (with-fixture fixed-header-fixture (0)
    (is (eql (mcutiet.message::reserved-bit2-p object) nil)))
  (with-fixture fixed-header-fixture (4)
    (is (eql (mcutiet.message::reserved-bit2-p object) t)))
  (with-fixture fixed-header-fixture (0)
    (is (eql (mcutiet.message::reserved-bit3-p object) nil)))
  (with-fixture fixed-header-fixture (8)
    (is (eql (mcutiet.message::reserved-bit3-p object) t))))


(test setf-reserved-bitn
  "Test methods `reserved-bitn`"

  (with-fixture fixed-header-fixture (0)
    ;; test bit0
    (setf (mcutiet.message::reserved-bit0 object) nil)
    (is (eql (mcutiet.message::reserved-bit0-p object) nil))
    (setf (mcutiet.message::reserved-bit0 object) t)
    (is (eql (mcutiet.message::reserved-bit0-p object) t))
    ;; test bit1
    (setf (mcutiet.message::reserved-bit1 object) nil)
    (is (eql (mcutiet.message::reserved-bit1-p object) nil))
    (setf (mcutiet.message::reserved-bit1 object) t)
    (is (eql (mcutiet.message::reserved-bit1-p object) t))
    ;; test bit2
    (setf (mcutiet.message::reserved-bit2 object) nil)
    (is (eql (mcutiet.message::reserved-bit2-p object) nil))
    (setf (mcutiet.message::reserved-bit2 object) t)
    (is (eql (mcutiet.message::reserved-bit2-p object) t))
    ;; test bit3
    (setf (mcutiet.message::reserved-bit3 object) nil)
    (is (eql (mcutiet.message::reserved-bit3-p object) nil))
    (setf (mcutiet.message::reserved-bit3 object) t)
    (is (eql (mcutiet.message::reserved-bit3-p object) t))))

(test encode-size
  "Test function `encode-size`"
  (is (equalp (encode-size 0) #(0)))
  (is (equalp (encode-size 268435455) #(255 255 255 127))))

(def-fixture decode-size-fixture (data)
  (let ((stream (make-instance 'mcutiet.streams:vector-bidirectional-stream
                               :data data)))
    (&body)))

(test decode-size
  "Test function `decode-size`"
  (with-fixture decode-size-fixture (#(255 255 255 127))
    (is (eql (decode-size stream) 268435455)))
  (with-fixture decode-size-fixture (#(0))
    (is (eql (decode-size stream) 0)))
  (with-fixture decode-size-fixture (#(255 255 255 128))
    (signals mcutiet.message::packet-length-too-big (decode-size stream))))

(test decode-size-no-hang
  "Test function `decode-size-no-hang`"
  (with-fixture decode-size-fixture (#(255 255 255 127))
    (let ((rl (make-instance 'mcutiet.message:remaining-length)))
      (is (decode-size-no-hang rl stream))
      (is (eql (mcutiet.message:total-number-of-bytes rl) 268435455))
      (is (eql (mcutiet.message:bytes-left rl) 268435455))))
  (with-fixture decode-size-fixture (#(255 255 255 127))
    (let ((rl (make-instance 'mcutiet.message:remaining-length)))
      (is (decode-size-no-hang rl stream))
      (is (eql (mcutiet.message:total-number-of-bytes rl) 268435455))
      (is (eql (mcutiet.message:bytes-left rl) 268435455))
      (setf (mcutiet.message:bytes-left rl) 100)
      (is (decode-size-no-hang rl stream))
      (is (eql (mcutiet.message:bytes-left rl) 100))))
  (with-fixture decode-size-fixture (#(0))
    (let ((rl (make-instance 'mcutiet.message:remaining-length)))
      (is (decode-size-no-hang rl stream))
      (is (= (mcutiet.message:total-number-of-bytes rl) 0))
      (is (eql (mcutiet.message:bytes-left rl) 0))))
  (with-fixture decode-size-fixture (#(255 255 255 128))
    (let ((rl (make-instance 'mcutiet.message:remaining-length)))
      (signals mcutiet.message::packet-length-too-big
        (decode-size-no-hang rl stream))
      (is (eql (mcutiet.message:bytes-left rl) nil))))
  (with-fixture decode-size-fixture (#(255 255 255))
    (let ((rl (make-instance 'mcutiet.message:remaining-length)))
      (is (not (decode-size-no-hang rl stream)))
      (is (eql (mcutiet.message:bytes-left rl) nil))
      (write-byte 127 stream)
      (setf (mcutiet.streams:stream-position stream) 3)
      (is (decode-size-no-hang rl stream))
      (is (eql (mcutiet.message:total-number-of-bytes rl) 268435455))
      (is (eql (mcutiet.message:bytes-left rl) 268435455))))
  (with-fixture decode-size-fixture (#(255 255 255))
    (let ((rl (make-instance 'mcutiet.message:remaining-length)))
      (is (not (decode-size-no-hang rl stream)))
      (is (eql (mcutiet.message:bytes-left rl) nil))
      (write-byte 128 stream)
      (setf (mcutiet.streams:stream-position stream) 3)
      (signals mcutiet.message::packet-length-too-big (decode-size-no-hang rl stream))
      (is (eql (mcutiet.message:bytes-left rl) nil))
      (signals mcutiet.message::packet-length-too-big (decode-size-no-hang rl stream)))))

(5am:in-suite message-tests)

(test packet-type
  "Test method `packet-type`"
  ;; Set header to :connect flag
  (with-fixture fixed-header-fixture (16)
    (is (eql (mcutiet.message::packet-type object) :connect)))
  ;; Set header to :disconnect flag
  (with-fixture fixed-header-fixture (224)
    (is (eql (mcutiet.message::packet-type object) :disconnect)))
  ;; Set header to invalid flag
  (with-fixture fixed-header-fixture (1)
    (signals mcutiet.message::invalid-packet-type-error
      (mcutiet.message::packet-type object)))
  ;; Set header to invalid flag
  (with-fixture fixed-header-fixture (240)
    (signals mcutiet.message::invalid-packet-type-error
      (mcutiet.message::packet-type object))))

(def-fixture set-packet-type-fixture (packet-type)
  (let ((object (make-instance 'mcutiet.message::fixed-header)))
    (setf (packet-type object) packet-type)
    (&body)))

(test setf-packet-type
  "Test method `set packet-type`"
  (with-fixture set-packet-type-fixture (:connect)
    (is (eql (packet-type object) :connect)))
  (with-fixture set-packet-type-fixture (:disconnect)
    (is (eql (packet-type object) :disconnect)))
  (with-fixture set-packet-type-fixture (:connect)
    (signals mcutiet.message::invalid-packet-type-error
      (setf (packet-type object) :fail))))

(def-fixture connect-fixture (slot data)
  (let ((object (allocate-instance (find-class 'mcutiet.message:connect))))
    (setf (slot-value object slot) data)
    (&body)))

(test will-flag
  "Test methods `setf will-flag` and `will-flag-p"
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 0)
    (is (eql (mcutiet.message:will-flag-p object) nil)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 4)
    (is (eql (mcutiet.message:will-flag-p object) t)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 4)
    (setf (mcutiet.message:will-flag object) nil)
    (is (eql (mcutiet.message:will-flag-p object) nil))
    (setf (mcutiet.message:will-flag object) t)
    (is (eql (mcutiet.message:will-flag-p object) t))))

(test will-retain
  "Test methods `setf will-retain` and `will-retain-p"
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 0)
    (is (eql (mcutiet.message:will-retain-p object) nil)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 32)
    (is (eql (mcutiet.message:will-retain-p object) t)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 32)
    (setf (mcutiet.message:will-retain object) nil)
    (is (eql (mcutiet.message:will-retain-p object) nil))
    (setf (mcutiet.message:will-retain object) t)
    (is (eql (mcutiet.message:will-retain-p object) t))))

(test will-qos
  "Test methods `setf will-qos` and `will-qos"
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 0)
    (is (eql (mcutiet.message:will-qos object) :at-most-once)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 8)
    (is (eql (mcutiet.message:will-qos object) :at-least-once)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 16)
    (is (eql (mcutiet.message:will-qos object) :exactly-once)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 24)
    (is (eql (mcutiet.message:will-qos object) :reserved)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 24)
    (setf (mcutiet.message:will-qos object) :at-most-once)
    (is (eql (mcutiet.message:will-qos object) :at-most-once))
    (setf (mcutiet.message:will-qos object) :at-least-once)
    (is (eql (mcutiet.message:will-qos object) :at-least-once))
    (setf (mcutiet.message:will-qos object) :exactly-once)
    (is (eql (mcutiet.message:will-qos object) :exactly-once))
    (setf (mcutiet.message:will-qos object) :reserved)
    (is (eql (mcutiet.message:will-qos object) :reserved))))

(test user-name-flag
  "Test methods `setf user-name-flag` and `user-name-p`"
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 0)
    (is (eql (mcutiet.message:user-name-p object) nil)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 128)
    (is (eql (mcutiet.message:user-name-p object) t)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 128)
    (setf (mcutiet.message:user-name-flag object) nil)
    (is (eql (mcutiet.message:user-name-p object) nil))
    (setf (mcutiet.message:user-name-flag object) t)
    (is (eql (mcutiet.message:user-name-p object) t))))

(test password-flag
  "Test methods `setf password-flag` and `password-p`"
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 0)
    (is (eql (mcutiet.message:password-p object) nil)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 64)
    (is (eql (mcutiet.message:password-p object) t)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 64)
    (setf (mcutiet.message:password-flag object) nil)
    (is (eql (mcutiet.message:password-p object) nil))
    (setf (mcutiet.message:password-flag object) t)
    (is (eql (mcutiet.message:password-p object) t))))

(test clean-session
  "Test methods `setf clean-session` and `clean-session-p`"
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 0)
    (is (eql (mcutiet.message:clean-session-p object) nil)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 2)
    (is (eql (mcutiet.message:clean-session-p object) t)))
  (with-fixture connect-fixture ('mcutiet.message::connect-flags 0)
    (setf (mcutiet.message:clean-session object) nil)
    (is (eql (mcutiet.message:clean-session-p object) nil))
    (setf (mcutiet.message:clean-session object) t)
    (is (eql (mcutiet.message:clean-session-p object) t))))

(def-fixture connack-fixture (slot data)
  (let ((object (allocate-instance (find-class 'mcutiet.message:connack))))
    (setf (slot-value object slot) data)
    (&body)))

(test session-present
  "Test methods `setf session-present` and `session-present-p`"
  (with-fixture connack-fixture ('mcutiet.message::acknowledge-flags 0)
    (is (eql (mcutiet.message:session-present-p object) nil)))
  (with-fixture connack-fixture ('mcutiet.message::acknowledge-flags 1)
    (is (eql (mcutiet.message:session-present-p object) t)))
  (with-fixture connack-fixture ('mcutiet.message::acknowledge-flags 1)
    (setf (mcutiet.message:session-present object) t)
    (is (eql (mcutiet.message:session-present-p object) t))
    (setf (mcutiet.message:session-present object) nil)
    (is (eql (mcutiet.message:session-present-p object) nil))))

(test connect-return-code
  "Test methods `setf connect-return-code` and `connect-return-code`"
  (with-fixture connack-fixture ('mcutiet.message::return-code-flags 0)
    (is (eql (mcutiet.message:connect-return-code object) :accepted)))
  (with-fixture connack-fixture ('mcutiet.message::return-code-flags 1)
    (is (eql (mcutiet.message:connect-return-code object) :refused-unacceptable-protocol-version)))
  (with-fixture connack-fixture ('mcutiet.message::return-code-flags 2)
    (is (eql (mcutiet.message:connect-return-code object) :refused-identifier-rejected)))
  (with-fixture connack-fixture ('mcutiet.message::return-code-flags 3)
    (is (eql (mcutiet.message:connect-return-code object) :refused-server-unavailable)))
  (with-fixture connack-fixture ('mcutiet.message::return-code-flags 4)
    (is (eql (mcutiet.message:connect-return-code object) :refused-bad-username)))
  (with-fixture connack-fixture ('mcutiet.message::return-code-flags 5)
    (is (eql (mcutiet.message:connect-return-code object) :refused-not-authorized)))
  (with-fixture connack-fixture ('mcutiet.message::return-code-flags 5)
    (setf (mcutiet.message:connect-return-code object) :accepted)
    (is (eql (mcutiet.message:connect-return-code object) :accepted))
    (setf (mcutiet.message:connect-return-code object) :refused-unacceptable-protocol-version)
    (is (eql (mcutiet.message:connect-return-code object) :refused-unacceptable-protocol-version))
    (setf (mcutiet.message:connect-return-code object) :refused-identifier-rejected)
    (is (eql (mcutiet.message:connect-return-code object) :refused-identifier-rejected))
    (setf (mcutiet.message:connect-return-code object) :refused-server-unavailable)
    (is (eql (mcutiet.message:connect-return-code object) :refused-server-unavailable))
    (setf (mcutiet.message:connect-return-code object) :refused-bad-username)
    (is (eql (mcutiet.message:connect-return-code object) :refused-bad-username))))

(def-fixture publish-fixture (slot data)
  (let ((object (allocate-instance (find-class 'mcutiet.message:publish))))
    (setf (slot-value object slot) data)
    (&body)))

(test qos
  "Test methods `setf qos` and `qos`"
  (with-fixture publish-fixture ('mcutiet.message::header 0)
    (is (eql (mcutiet.message:qos object) :at-most-once)))
  (with-fixture publish-fixture ('mcutiet.message::header 2)
    (is (eql (mcutiet.message:qos object) :at-least-once)))
  (with-fixture publish-fixture ('mcutiet.message::header 4)
    (is (eql (mcutiet.message:qos object) :exactly-once)))
  (with-fixture publish-fixture ('mcutiet.message::header 6)
    (is (eql (mcutiet.message:qos object) :reserved)))
  (with-fixture publish-fixture ('mcutiet.message::header 6)
    (setf (mcutiet.message:qos object) :at-most-once)
    (is (eql (mcutiet.message:qos object) :at-most-once))
    (setf (mcutiet.message:qos object) :at-least-once)
    (is (eql (mcutiet.message:qos object) :at-least-once))
    (setf (mcutiet.message:qos object) :exactly-once)
    (is (eql (mcutiet.message:qos object) :exactly-once))
    (setf (mcutiet.message:qos object) :reserved)
    (is (eql (mcutiet.message:qos object) :reserved))))

(test dup
  "Test methods `setf dup` and `dup-p`"
  (with-fixture publish-fixture ('mcutiet.message::header 0)
    (is (eql (mcutiet.message:dup-p object) nil)))
  (with-fixture publish-fixture ('mcutiet.message::header 8)
    (is (eql (mcutiet.message:dup-p object) t)))
  (with-fixture publish-fixture ('mcutiet.message::header 8)
    (setf (mcutiet.message:dup object) nil)
    (is (eql (mcutiet.message:dup-p object) nil))
    (setf (mcutiet.message:dup object) t)
    (is (eql (mcutiet.message:dup-p object) t))))

(test retain
  "Test methods `setf retain` and `retain`"
  (with-fixture publish-fixture ('mcutiet.message::header 0)
    (is (eql (mcutiet.message:retain-p object) nil)))
  (with-fixture publish-fixture ('mcutiet.message::header 1)
    (is (eql (mcutiet.message:retain-p object) t)))
  (with-fixture publish-fixture ('mcutiet.message::header 1)
    (setf (mcutiet.message:retain object) nil)
    (is (eql (mcutiet.message:retain-p object) nil))
    (setf (mcutiet.message:retain object) t)
    (is (eql (mcutiet.message:retain-p object) t))))

(def-fixture subscribe-fixture (slot data)
  (let ((object (allocate-instance (find-class 'mcutiet.message:subscribe))))
    (setf (slot-value object slot) data)
    (&body)))

(test requested-qos
  "Test methods `setf requested-qos` and `requested-qos`"
  (with-fixture subscribe-fixture ('mcutiet.message::requested-qos-flags 0)
    (is (eql (mcutiet.message:requested-qos object) :at-most-once)))
  (with-fixture subscribe-fixture ('mcutiet.message::requested-qos-flags 1)
    (is (eql (mcutiet.message:requested-qos object) :at-least-once)))
  (with-fixture subscribe-fixture ('mcutiet.message::requested-qos-flags 2)
    (is (eql (mcutiet.message:requested-qos object) :exactly-once)))
  (with-fixture subscribe-fixture ('mcutiet.message::requested-qos-flags 3)
    (is (eql (mcutiet.message:requested-qos object) :reserved)))
  (with-fixture subscribe-fixture ('mcutiet.message::requested-qos-flags 3)
    (setf (mcutiet.message:requested-qos object) :at-most-once)
    (is (eql (mcutiet.message:requested-qos object) :at-most-once))
    (setf (mcutiet.message:requested-qos object) :at-least-once)
    (is (eql (mcutiet.message:requested-qos object) :at-least-once))
    (setf (mcutiet.message:requested-qos object) :exactly-once)
    (is (eql (mcutiet.message:requested-qos object) :exactly-once))
    (setf (mcutiet.message:requested-qos object) :reserved)
    (is (eql (mcutiet.message:requested-qos object) :reserved))))

(def-fixture suback-fixture (slot data)
  (let ((object (allocate-instance (find-class 'mcutiet.message:suback))))
    (setf (slot-value object slot) data)
    (&body)))


(test suback-return-code
  "Test methods `setf suback-return-code` and `suback-return-code`"
  (with-fixture suback-fixture ('mcutiet.message::return-code-flags 0)
    (is (eql (mcutiet.message:suback-return-code object) :at-most-once)))
  (with-fixture suback-fixture ('mcutiet.message::return-code-flags 1)
    (is (eql (mcutiet.message:suback-return-code object) :at-least-once)))
  (with-fixture suback-fixture ('mcutiet.message::return-code-flags 2)
    (is (eql (mcutiet.message:suback-return-code object) :exactly-once)))
  (with-fixture suback-fixture ('mcutiet.message::return-code-flags 128)
    (is (eql (mcutiet.message:suback-return-code object) :failure)))
  (with-fixture suback-fixture ('mcutiet.message::return-code-flags 128)
    (setf (mcutiet.message:suback-return-code object) :at-most-once)
    (is (eql (mcutiet.message:suback-return-code object) :at-most-once))
    (setf (mcutiet.message:suback-return-code object) :at-least-once)
    (is (eql (mcutiet.message:suback-return-code object) :at-least-once))
    (setf (mcutiet.message:suback-return-code object) :exactly-once)
    (is (eql (mcutiet.message:suback-return-code object) :exactly-once))
    (setf (mcutiet.message:suback-return-code object) :failure)
    (is (eql (mcutiet.message:suback-return-code object) :failure))))

(def-fixture packet-fixture (type slots)
  (let ((object (allocate-instance (find-class type))))
    (loop for slot in slots do
      (setf (slot-value object (first slot)) (second slot)))
    (&body)))

(test valid-reserved-bits
  "Test method `valid-reserved-bits`"
  ;; Test connect package
  (with-fixture packet-fixture ('mcutiet.message:connect
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:connect
                                '((mcutiet.message::header 1)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test connack package
  (with-fixture packet-fixture ('mcutiet.message:connack
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:connack
                                '((mcutiet.message::header 8)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test publish package
  (with-fixture packet-fixture ('mcutiet.message:publish
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:publish
                                '((mcutiet.message::header 2)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:publish
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test puback package
  (with-fixture packet-fixture ('mcutiet.message:puback
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:puback
                                '((mcutiet.message::header 4)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test pubrec
  (with-fixture packet-fixture ('mcutiet.message:pubrec
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:pubrec
                                '((mcutiet.message::header 8)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test pubrel
  (with-fixture packet-fixture ('mcutiet.message:pubrel
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:pubrel
                                '((mcutiet.message::header 2)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:pubrel
                                '((mcutiet.message::header 8)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test pubcomp
  (with-fixture packet-fixture ('mcutiet.message:pubcomp
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:pubcomp
                                '((mcutiet.message::header 8)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test subscribe
  (with-fixture packet-fixture ('mcutiet.message:subscribe
                                '((mcutiet.message::header 0)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  (with-fixture packet-fixture ('mcutiet.message:subscribe
                                '((mcutiet.message::header 2)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:subscribe
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  (with-fixture packet-fixture ('mcutiet.message:subscribe
                                '((mcutiet.message::header 3)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test suback
  (with-fixture packet-fixture ('mcutiet.message:suback
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:suback
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test unsubscribe
  (with-fixture packet-fixture ('mcutiet.message:unsubscribe
                                '((mcutiet.message::header 0)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  (with-fixture packet-fixture ('mcutiet.message:unsubscribe
                                '((mcutiet.message::header 2)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:unsubscribe
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  (with-fixture packet-fixture ('mcutiet.message:unsubscribe
                                '((mcutiet.message::header 3)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test unsuback
  (with-fixture packet-fixture ('mcutiet.message:unsuback
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:unsuback
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test pingreq
  (with-fixture packet-fixture ('mcutiet.message:pingreq
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:pingreq
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test pingresp
  (with-fixture packet-fixture ('mcutiet.message:pingresp
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:pingresp
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object))))
  ;; Test disconnect
  (with-fixture packet-fixture ('mcutiet.message:disconnect
                                '((mcutiet.message::header 0)))
    (is (mcutiet.message::valid-reserved-bits object)))
  (with-fixture packet-fixture ('mcutiet.message:disconnect
                                '((mcutiet.message::header 6)))
    (is (not (mcutiet.message::valid-reserved-bits object)))))
  

(test valid-packet-connect "Test valid-packet for connect"
  ;; Test unset client-id
  (with-fixture packet-fixture ('mcutiet.message:connect
                                '((mcutiet.message::client-id nil)))
    (is (not (mcutiet.message::valid-packet object)) "Client-id set"))
  ;; Test for wrong protocol-name
  (with-fixture packet-fixture ('mcutiet.message:connect
                                '((mcutiet.message::client-id "id")
                                  (mcutiet.message::protocol-name "bla")))
    (is (not (mcutiet.message::valid-packet object)) "Wrong protocol name"))
  ;; Test will-flags
  ;; (with-fixture packet-fixture ('mcutiet.message:connect
  ;;                               '((mcutiet.message::client-id "id")
  ;;                                 (mcutiet.message::protocol-name "MQTT")))
  ;;   (is (not (mcutiet.message::valid-packet object)) "Wrong protocol name"))
  )
