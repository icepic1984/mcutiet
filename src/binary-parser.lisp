;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.

;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.

;;     * Neither the name of Gigamonkeys Consulting nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :mcutiet.binary-parser
  (:use :cl)
  (:import-from :alexandria :with-gensyms)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value))

(in-package :mcutiet.binary-parser)

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun mklist (x)
  (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'supperclass)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclass)
  (nconc (mapcan #'all-slots superclass) (mapcar #'first slots)))

(defun slot->defclass-slot (spec)
  (let* ((name (first spec))
         (keyword (as-keyword name)))
    `(,name :accessor ,name :initarg ,keyword :initform nil)))

(defun slot->initialize (spec object)
  (destructuring-bind (name &rest args) (normalize-slot-spec spec)
    (declare (ignore args))
    `(setf (slot-value ,object ',name) ,name )))

(defun slots->initialize (slots object)
  (mapcar (lambda (x) (slot->initialize x object)) slots))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

(defgeneric read-value (type stream &key &allow-other-keys))

(defgeneric write-value (type stream value &key &allow-other-keys))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last))

(defmethod read-value ((type symbol) stream &key &allow-other-keys)
  (let ((object (allocate-instance (find-class type))))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key &allow-other-keys)
  (assert (typep value type))
  (write-object value stream))

(defmacro define-generic-binary-class (name (&rest superclass) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn

       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclass) ',superclass))

       (defclass ,name ,superclass
         ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclass) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclass) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class
         ,name
         ,superclass
         ,slots
         (defmethod read-object progn ((,objectvar ,name) ,streamvar)
           (declare (ignorable ,streamvar))
           (with-slots ,(new-class-all-slots slots superclass) ,objectvar
             ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-binary-class (name (&rest superclass) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class
         ,name
         ,superclass
         ,slots
         (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
           (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
             (let ((,objectvar (allocate-instance (find-class
                                                   ,@(or (cdr (assoc :dispatch options))
                                                         (error "Must supply :dispatch form"))))))
               ,@(slots->initialize slots objectvar)
               (read-object ,objectvar ,streamvar)
               ,objectvar))))))


(defmacro define-binary-type (name (&rest args) &body spec)
  (with-gensyms (type)
    (ecase (length spec)
      (1
       (with-gensyms (type stream value)
         (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
           `(progn
              (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
                (declare (ignorable ,@args))
                (read-value ',derived-from ,stream ,@derived-args))
              (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
                (declare (ignorable ,@args))
                (write-value ',derived-from ,stream ,value ,@derived-args))))))
      (2
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                (declare (ignorable ,@args))
                ,@body))
          ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
             `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
                (declare (ignorable ,@args))
                ,@body)))))))

