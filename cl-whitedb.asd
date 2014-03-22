(in-package :cl)
(defpackage cl-whitedb-asd (:use :cl :asdf))
(in-package :cl-whitedb-asd)

(defsystem cl-whitedb
  :version "0.1"
  :author "Oleg Sivokon <olegsivokon@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria :iterate :cl-containers :cffi)
  :components ((:module "src" :serial t
                        :components
                        ((:file "package")
                         (:file "bindings" :depends-on ("package"))
                         (:module "structures" :serial t :depends-on ("bindings")
                                  :components
                                  ((:file "package")
                                   (:file "structures" :depends-on ("package"))))
                         (:module "search" :serial t :depends-on ("bindings")
                                  :components
                                  ((:file "package")
                                   (:file "uninformed" :depends-on ("package"))
                                   (:file "informed" :depends-on ("package"))))
                         (:module "zipper" :serial t :depends-on ("bindings")
                                  :components
                                  ((:file "package")
                                   (:file "zipper" :depends-on ("package"))))
                         (:module "sequences" :serial t :depends-on ("bindings")
                                  :components
                                  ((:file "package")
                                   (:file "sequences" :depends-on ("package")))))))
  :description "Bindings and some goodies fro WhiteDB"
  :long-description
  #.(with-open-file
        (stream (merge-pathnames
                 #p"README.org" (or *load-pathname* *compile-file-pathname*))
                :if-does-not-exist nil :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream)) seq)))
  :in-order-to ((test-op (load-op :cl-whitedb-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:run!) :cl-whitedb.test)
                             :cl-whitedb.test)))

(defsystem :cl-whitedb-test
  :author "Oleg Sivokon <olegsivokon@gmail.com>"
  :description "Minimal test suite for testing cl-whitedb"
  :license "MIT"
  :depends-on (:agraph-client :fiveam)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "suite" :depends-on ("package"))
                         (:file "test-bindings" :depends-on ("suite"))))))
