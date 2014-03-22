;; -*- mode: lisp -*-
(in-package :cl-whitedb.sequences)

(defun map-records (database result-type function)
  (iter
    (for record :initially (first-record (database-handle database))
         :then (next-record (database-handle database)
                            :previous-record record
                            :nothrow :nothrow))
    (until (eql record :nothrow))
    (collect (funcall function record) :into result)
    (finally
     (return (map result-type #'identity result)))))

(defun reduce-records (database function &key initial-value)
  (iter
    (for record :initially (first-record (database-handle database))
         :then (next-record (database-handle database)
                            :previous-record record
                            :nothrow :nothrow))
    (until (eql record :nothrow))
    (reducing record :by function :initial-value initial-value)))

(defun find-record-if (database function default)
  (iter
    (for record :initially (first-record (database-handle database))
         :then (next-record (database-handle database)
                            :previous-record record
                            :nothrow :nothrow))
    (until (eql record :nothrow))
    (finding record :such-that (funcall function record)
             :on-failure default)))

(defun find-record-if (database function default)
  (iter
    (for record :initially (first-record (database-handle database))
         :then (next-record (database-handle database)
                            :previous-record record
                            :nothrow :nothrow))
    (until (eql record :nothrow))
    (finding record :such-that (funcall function record)
             :on-failure default)))

(defun count-records (database)
  (iter
    (for record :initially (first-record (database-handle database))
         :then (next-record (database-handle database)
                            :previous-record record
                            :nothrow :nothrow))
    (until (eql record :nothrow))
    (counting record)))
