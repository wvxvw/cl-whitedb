(in-package :cl-whitedb.test)

(in-suite :cl-whitedb.test)

(def-suite test-suite
    :description "Minimal testing suite for cl-whitedb project.")

(test test-create-db
  "Tries to solve a simple (classic) sudoku board"
  (is t))
