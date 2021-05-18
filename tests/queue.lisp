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

(5am:in-suite queue-tests)

(def-fixture queue-fixture ()
  (let ((queue (make-instance 'mcutiet.queue:message-queue)))
    (&body)))

(test message-queue-enqueue "Test enqueue on message-queue"
  (with-fixture queue-fixture ()
    (is (mcutiet.queue:empty-p queue))
    (mcutiet.queue:enqueue queue 'a)
    (is (= (mcutiet.queue:len queue) 1))
    (mcutiet.queue:enqueue queue 'b)
    (is (= (mcutiet.queue:len queue) 2))))

(test message-queue-empty-p "Test empty-p on message-queue"
  (with-fixture queue-fixture ()
    (is (mcutiet.queue:empty-p queue))
    (mcutiet.queue:enqueue queue 'a)
    (is (not (mcutiet.queue:empty-p queue)))))

(test message-queue-len "Test len on message-queue"
  (with-fixture queue-fixture ()
    (is (= 0 (mcutiet.queue:len queue)))
    (mcutiet.queue:enqueue queue 'a)
    (is (= 1 (mcutiet.queue:len queue)))
    (mcutiet.queue:dequeue queue)
    (is (= 0 (mcutiet.queue:len queue)))))

(test message-queue-dequeue "Test dequeue on message-queue"
  (with-fixture queue-fixture ()
    (is (mcutiet.queue:empty-p queue))
    (is (= 0 (mcutiet.queue:len queue)))
    (mcutiet.queue:enqueue queue 'a)
    (mcutiet.queue:enqueue queue 'b)
    (is (not (mcutiet.queue:empty-p queue)))
    (is (= 2 (mcutiet.queue:len queue)))
    (is (eql 'a (mcutiet.queue:dequeue queue)))
    (is (not (mcutiet.queue:empty-p queue)))
    (is (= 1 (mcutiet.queue:len queue)))
    (is (eql 'b (mcutiet.queue:dequeue queue)))
    (is (mcutiet.queue:empty-p queue))
    (is (= 0 (mcutiet.queue:len queue)))
    (mcutiet.queue:enqueue queue nil)
    (is (= 1 (mcutiet.queue:len queue)))
    (is (not (mcutiet.queue:empty-p queue)))
    (is (eql nil (mcutiet.queue:dequeue queue)))))
  
(test message-queue-dequeue-no-hang "Test dequeue-no-hang on message-queue"
  (with-fixture queue-fixture ()
    (is (mcutiet.queue:empty-p queue))
    (is (= 0 (mcutiet.queue:len queue)))
    (mcutiet.queue:enqueue queue 'a)
    (mcutiet.queue:enqueue queue 'b)
    (is (not (mcutiet.queue:empty-p queue)))
    (is (= 2 (mcutiet.queue:len queue)))
    (is (eql 'a (mcutiet.queue:dequeue-no-hang queue)))
    (is (not (mcutiet.queue:empty-p queue)))
    (is (= 1 (mcutiet.queue:len queue)))
    (is (eql 'b (mcutiet.queue:dequeue-no-hang queue)))
    (is (mcutiet.queue:empty-p queue))
    (is (= 0 (mcutiet.queue:len queue)))
    (mcutiet.queue:enqueue queue nil)
    (is (= 1 (mcutiet.queue:len queue)))
    (is (not (mcutiet.queue:empty-p queue)))
    (multiple-value-bind (a b) (mcutiet.queue:dequeue-no-hang queue)
      (is (and (eql nil a)
               (eql t b))))
    (is (= 0 (mcutiet.queue:len queue)))
    (is (mcutiet.queue:empty-p queue))
    (multiple-value-bind (a b) (mcutiet.queue:dequeue-no-hang queue)
      (is (and (eql nil a)
               (eql nil b))))
    (is (= 0 (mcutiet.queue:len queue)))
    (is (mcutiet.queue:empty-p queue))))

    
  


