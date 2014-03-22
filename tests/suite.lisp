(in-package :cl-whitedb.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :cl-whitedb)
    (def-suite :cl-whitedb)))

(def-suite :cl-whitedb.test :in :cl-whitedb)
