(cl:defpackage :chillax.utils
  (:use :cl :alexandria)
  (:export
   :fun :mkhash :hashget :strcat :dequote :at))
(in-package :chillax.utils)

;;; Functions
(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

;;; Hash tables
(defun mkhash (&rest keys-and-values &aux (table (make-hash-table :test #'equal)))
  "Convenience function for `literal' hash table definition."
  (loop for (key val) on keys-and-values by #'cddr do (setf (gethash key table) val)
     finally (return table)))

(defun hashget (hash &rest keys)
  "Convenience function for recursively accessing hash tables."
  (reduce (lambda (h k) (gethash k h)) keys :initial-value hash))

(define-compiler-macro hashget (hash &rest keys)
  (if (null keys) hash
      (let ((hash-sym (make-symbol "HASH"))
            (key-syms (loop for i below (length keys)
                         collect (make-symbol (format nil "~:@(~:R~)-KEY" i)))))
        `(let ((,hash-sym ,hash)
               ,@(loop for key in keys for sym in key-syms
                    collect `(,sym ,key)))
           ,(reduce (lambda (hash key) `(gethash ,key ,hash))
                    key-syms :initial-value hash-sym)))))

(defun (setf hashget) (new-value hash key &rest more-keys)
  "Uses the last key given to hashget to insert NEW-VALUE into the hash table
returned by the second-to-last key.
tl;dr: DWIM SETF function for HASHGET."
  (if more-keys
      (setf (gethash (car (last more-keys))
                     (apply #'hashget hash key (butlast more-keys)))
            new-value)
      (setf (gethash key hash) new-value)))

;;; Strings
(defun strcat (string &rest more-strings)
  (apply #'concatenate 'string string more-strings))

(defun dequote (string)
  (let ((len (length string)))
    (if (and (> len 1) (starts-with #\" string) (ends-with #\" string))
      (subseq string 1 (- len 1))
      string)))

;;;
;;; At
;;;
(defgeneric at (doc &rest keys))
(defgeneric (setf at) (new-value doc key &rest more-keys))

(defmethod at ((doc hash-table) &rest keys)
  (apply #'hashget doc keys))
(defmethod (setf at) (new-value (doc hash-table) key &rest more-keys)
  (apply #'(setf hashget) new-value doc key more-keys))

(defmethod at ((doc list) &rest keys)
  (reduce (lambda (alist key)
            (cdr (assoc key alist :test #'equal)))
          keys :initial-value doc))
(defmethod (setf at) (new-value (doc list) key &rest more-keys)
  (if more-keys
      (setf (cdr (assoc (car (last more-keys))
                        (apply #'at doc key (butlast more-keys))
                        :test #'equal))
            new-value)
      (setf (cdr (assoc key doc :test #'equal)) new-value)))

;; A playful alias.
(defun @ (doc &rest keys)
  (apply #'at doc keys))
(defun (setf @) (new-value doc key &rest more-keys)
  (apply #'(setf at) new-value doc key more-keys))
