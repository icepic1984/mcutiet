(defpackage mcutiet/tests/main
  (:use :cl :mcutiet :rove))

(in-package :mcutiet/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :mcutiet)' in your Lisp.

(deftest packet-type-to-raw-test
  (testing "pass_:connect_return_1"
    (ok (= (mcutiet2::packet-type-to-raw :connect) 1)))
  (testing "pass_:disconnect_return_14"
    (ok (= (mcutiet2::packet-type-to-raw :disconnect) 14)))
  (testing "pass_:invalid_signals_invalid-packet-type-error"
    (ok (signals (mcutiet2::packet-type-to-raw :invalid)
            'mcutiet2::invalid-packet-type-error) "Invalid packet type")))

(deftest packet-type-from-raw-test
  (testing "pass_1_return_:connect"
    (ok (eql (mcutiet2::packet-type-from-raw 1) :connect)))
  (testing "pass_14_return_:disconnect"
    (ok (eql (mcutiet2::packet-type-from-raw 14) :disconnect)))
  (testing "pass_0_signal_invalid-packet-type-error"
    (ok (signals (mcutiet2::packet-type-from-raw 0)
            'mcutiet2::invalid-packet-type-error) "Invalid packet type"))
  (testing "pass_15_signal_invalid-packet-type-error"
    (ok (signals (mcutiet2::packet-type-from-raw 15)
            'mcutiet2::invalid-packet-type-error) "Invalid packet type"))
  (testing "pass_20_signal_invalid-packet-type-error"
      (ok (signals (mcutiet2::packet-type-from-raw 20)
            'mcutiet2::invalid-packet-type-error) "Invalid packet type")))

(deftest qos-from-raw-test
  (testing "pass_0_return_:at-most-once"
    (ok (eql (mcutiet2::qos-from-raw 0) :at-most-once)))
  (testing "pass_1_return_:at-least-once"
    (ok (eql (mcutiet2::qos-from-raw 1) :at-least-once)))
  (testing "pass_2_return_:exactly-once"
    (ok (eql (mcutiet2::qos-from-raw 2 ) :exactly-once)))
  (testing "pass_3_return_:reserved"
    (ok (eql (mcutiet2::qos-from-raw 3) :reserved)))
  (testing "pass_4_signals_invalid-qos-error"
    (ok (signals (mcutiet2::qos-from-raw 4)
            'mcutiet2::invalid-qos-error) "Invalid qos setting")))

(deftest qos-to-raw-test
  (testing "pass_:at-most-once_return_0"
    (ok (eql (mcutiet2::qos-to-raw :at-most-once) 0)))
  (testing "pass_:at-least-once_return_1"
    (ok (eql (mcutiet2::qos-to-raw :at-least-once) 1)))
  (testing "pass_:exactly-once_return_2"
    (ok (eql (mcutiet2::qos-to-raw :exactly-once ) 2)))
  (testing "pass_:reserved_return_3"
    (ok (eql (mcutiet2::qos-to-raw :reserved) 3)))
  (testing "pass_:invalid_signals_invalid-qos-error"
    (ok (signals (mcutiet2::qos-to-raw :invalid)
            'mcutiet2::invalid-qos-error) "Invalid qos setting")))

(deftest %qos-test
  (testing "read_:at-most-once"
    (ok (eql (mcutiet2::%qos #b0000 1) :at-most-once)))
  (testing "read_:at-least-once"
    (ok (eql (mcutiet2::%qos #b0010 1) :at-least-once)))
  (testing "read_:exactly-once"
    (ok (eql (mcutiet2::%qos #b0100 1) :exactly-once)))
  (testing "read_:reserved"
    (ok (eql (mcutiet2::%qos #b0110 1) :reserved))))

(deftest %dup-p
  (testing "dup_set_return_t"
    (ok (eql (mcutiet2::%dup-p #b1000) t)))
  (testing "dup_not_set_return_nil"
    (ok (eql (mcutiet2::%dup-p #b0000) nil))))

(deftest %retain-p
  (testing "retain_set_return_t"
    (ok (eql (mcutiet2::%retain-p #b0001) t)))
  (testing "retain_not_set_return_nil"
    (ok (eql (mcutiet2::%retain-p #b0000) nil))))

(deftest %packet-type
  (testing "connect_packet_set_return_:connect"
    (ok (eql (mcutiet2::%packet-type 16) :connect)))
  (testing "disconnect_packet_set_return_:disconnect"
    (ok (eql (mcutiet2::%packet-type 224) :disconnect)))
  (testing "invalid_packet_signals_invalid-packet-type-error"
    (ok (signals (mcutiet2::%packet-type 15)
            'mcutiet2::invalid-packet-type-error) "Invalid packet type")))

(let ((object (make-instance 'mcutiet2::fixed-header)))
  (deftest packet-type
    (testing "invalid_packet_type_signals_invalid-packet-type-error"
      (ok (signals (mcutiet2::packet-type object)
              'mcutiet2::invalid-packet-type-error) "Invalid packet type"))
    (testing ":connect_fixed-header_return_:connect"
      (with-slots (mcutiet2::header) object
        (setf mcutiet2::header #b00010000))
      (ok (eql (mcutiet2::packet-type object) :connect)))
    (testing ":disconnect_fixed-header_return_:disconnect"
      (with-slots (mcutiet2::header) object
        (setf mcutiet2::header #b11100000))
      (ok (eql (mcutiet2::packet-type object) :disconnect)))
    (testing "invalid_packet_type_signals-invalid-packet-type-error"
      (with-slots (mcutiet2::header) object
        (setf mcutiet2::header #b11110000))
      (ok (signals (mcutiet2::packet-type object)
                  'mcutiet2::invalid-packet-type-error) "Invalid packet type"))))



;; (defparameter *connack* (make-instance 'mcutiet2::connack ))
(defparameter *subscribe* (make-instance 'mcutiet2::subscribe))

(mcutiet2::requested-qos *subscribe*)
(setf (mcutiet2::requested-qos *subscribe*) :at-least-once)

;; (setf (mcutiet2::return-code *connack*) :invalid)
;; (mcutiet2::return-code *connack*)

;; (with-slots (mcutiet2::return-code) *connack*
;;   (setf mcutiet2::return-code 4 ))

;; (mcutiet2::return-code *connack*)

;; (mcutiet2::return-code *connack*)

;; (mcutiet2::acknowledge-flags *connack*)
;; (mcutiet2::return-code *connack*)
;; (mcutiet2::return-code-flags *connack*)
