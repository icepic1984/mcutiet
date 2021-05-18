(in-package :cl)

(defpackage :mcutiet.queue
  (:use
   :cl
   :bt)
  (:export
   :len
   :message-queue
   :empty-p
   :enqueue
   :dequeue
   :dequeue-no-hang))

(in-package :mcutiet.queue)


(defclass message-queue ()
  ((messages :initform nil)
   (last-cons :initform nil)
   (len :initform 0)
   (lock :initform (bt:make-lock))
   (flag :initform (bt:make-condition-variable))))

(defmethod len ((queue message-queue))
  (with-slots (lock len) queue
      (with-lock-held (lock)
        len)))

(defmethod empty-p ((queue message-queue))
  (with-slots (lock len) queue
    (with-lock-held (lock)
      (= len 0))))

(defmethod enqueue ((queue message-queue) object)
  "Adds an element to the back of the given queue in a thread-safe way."
  (with-slots (lock messages len last-cons flag) queue
    (with-lock-held (lock)
      (let ((o (list object)))
        (cond ((not messages)
               (setf messages o
                     last-cons messages
                     len 1))
              (t (setf (cdr last-cons) o
                       last-cons o)
                 (incf len)))))
    (condition-notify flag)
    messages))

(defmethod dequeue ((queue message-queue) &optional (timeout 0))
  "Pops a message from the given queue in a thread-safe way.
If the target queue is empty, blocks until a message arrives.
If timeout is not zero, errors after timeout."
  (with-slots (messages lock flag len) queue
    (with-timeout (timeout)
      (with-lock-held (lock)
        (unless messages (condition-wait flag lock))
        (decf len)
        (pop messages)))))

(defmethod dequeue-no-hang ((queue message-queue))
  "Pops a message from the given queue in a thread-safe way.
If the target queue is empty, returns NIL.
The second value specifies whether an item was found in queue (this is meant
to disambiguate the situation where a queue contains the message NIL)"
  (with-slots (messages lock flag len) queue
    (with-lock-held (lock)
      (if messages
      (progn
        (decf len)
        (values (pop messages) t))
      (values nil nil)))))



