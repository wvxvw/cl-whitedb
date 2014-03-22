;; -*- mode: lisp -*-
(in-package :cl-whitedb.structures)

(define-condition database-error (error)
  ((db :initarg :db :reader database-error-db)
   (message :initarg :message :reader database-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~S: ~S."
             (database-error-db condition)
             (database-error-message condition)))))

(defclass database ()
  ((file :initarg :file :accessor database-file)
   (size :initarg :size :accessor database-size :initform 1048576)
   (key :initarg :key :accessor database-key)
   (localp :initarg :localp :accessor database-local-p
           :documentation "If this is true, then this database
can only be used inside a single process (can't be share)")
   (logging :initarg :logging :initform nil :accessor database-logging)
   (logging-size :initarg :logging-size
                 :initform 1048576 :accessor database-logging-size)
   (db :accessor database-handle)))

(defmethod initialize-instance :after
    ((this database) &rest initargs
     &key lambda-list argument-precence-order &allow-other-keys)
  (declare (ignore lambda-list argument-precence-order))
  (destructuring-bind
        (&key key file size logging logging-size localp &allow-other-keys) initargs
    (when (and file key)
      (error 'database-error :db nil :message "Must only specify :file or :key"))
    (when file
      (let* ((file-name (namestring (translate-logical-pathname file)))
             (name-length (1+ (length file-name)))
             (key "1000")
             (handle (wg-attach-database key 0)))
        (when (null-pointer-p handle)
          (error 'database-error :db handle
                 :message "Failed to create database"))
        (setf (database-handle this) handle)
        (case (wg-import-dump
               (database-handle this)
               (with-foreign-pointer-as-string (buffer name-length)
                 (lisp-string-to-foreign file-name buffer name-length)))
          (0 (setf (database-file this) file))
          (-1
           (wg-delete-database (database-handle this))
           (setf (database-handle this) nil)
           (error 'database-error :db nil :message "Failed to load dump"))
          (-2
           (wg-delete-database (database-handle this))
           (setf (database-handle this) nil)
           (error 'database-error :db nil :message "Database is corrupt")))))
    (if key
        (let* ((keylength (1+ (length key)))
               (key                     ; who frees these strings?
                (with-foreign-pointer-as-string (buffer keylength)
                  (lisp-string-to-foreign key buffer keylength)))
               (handle
                (if size
                    (if localp
                        (wg-attach-local-database key)
                        (wg-attach-database key size))
                    (wg-attach-existing-database key))))
          (when (null-pointer-p handle)
            (error 'database-error :db handle
                   :message "Failed to create database"))
          (setf (database-handle this) handle)
          (when logging
            (let* ((keylength (1+ (length logging)))
                   (logging
                    (with-foreign-pointer-as-string (buffer keylength)
                      (lisp-string-to-foreign key buffer keylength)))
                   (handle (wg-attach-logged-database logging logging-size)))
              (when (null-pointer-p handle)
                (wg-detach-database (database-handle this))
                (error 'database-condition :db nil
                       :message "Failed to create logging database"))
              (setf (database-logging-size this) handle))))
        (error 'database-error :db nil
               :message ":key must not be nil"))))

(defmethod create-cursor ((this database))
  (make-cursor :handle (database-handle this)))

(defmethod dump ((this database) &optional (file (database-file this)))
  (let* ((file-name (namestring (translate-logical-pathname file)))
         (name-length (1+ (length file))))
    (with-foreign-pointer-as-string (buffer name-length)
      (case (wg-dump (database-handle this) 
                     (lisp-string-to-foreign file-name buffer name-length))
        (0 (setf (database-file this) file))
        (-1 (error 'database-error :db (database-handle this)
                   :message
                   (format nil "Couldn't dump database (check file path) ~s"
                           file)))
        (-2 (error 'database-error :db (database-handle this)
                   :message "Database must be corrup, try restoring from journal"))))))

(defmethod db-size ((this database))
  (wg-database-size (database-handle this)))

(defmethod db-free-size ((this database))
  (wg-database-freesize (database-handle this)))

;; TODO: Printing, currently C code prints to stdout
;; I will need to write some glue code, which prints to a buffer
;; it gets from lisp instead.

;; Example definition:
;; (define-record (name :extends (other records) :indexed t)
;;     ((field-a :initform 42 :type :wg-integer :accessor name-field-a)
;;      (field-b :initform "some string" :type :wg-string))
;;   "Docstring for name")

(defun encode-value (database value)
  (etypecase value
    (fixnum (wg-encode-int database value))
    (string
     (let ((strlen (length value)))
       (wg-encode-int
        database
        (with-foreign-pointer-as-string (buffer strlen)
          (lisp-string-to-foreign value buffer strlen)))))))

(defun defrecord-init-slots (kvargs record)
  (iter
    (with (database . record) := record)
    (with handle := (database-handle database))
    (for value :in-sequence kvargs :with-index i)
    (when value
      (let ((result
             (wg-set-field handle record i (encode-value value))))
        (when (< result 0)
          (error 'database-error :db handle
                 :message
                 (format nil "Couldn't set slot ~d on record ~s" i record)))))))

(defun make-defrecord-func (name suffix)
  (intern
   (format nil (if (eql suffix :make)
                   "MAKE~:@(~A~)~*"
                   "~:@(~A~)-~:@(~A~)")
           name suffix)))

(defstruct defrecord-slot name initform type accessor)

(defstruct defrecord-name
  name (extends nil) (indexed nil))

(defun make-defrecord-lambda-list (arguments)
  `((database
     raw size &key
     ,@(iter
        (for slot :in arguments)
        (for initform := (defrecord-slot-initform slot))
        (for name := (defrecord-slot-name slot))
        (collect (if initform (list name initform) name))))))

(defun parse-defrecord-slots (slots)
  (iter
    (for (name &key initform type accessor) :in slots)
    (collect (make-defrecord-slot
              :name name
              :initform initform
              :type type
              :accessor accessor))))

(defun make-defrecord-constructor (name arguments)
  `(defun ,(make-defrecord-func name :make)
       ,(make-defrecord-lambda-list arguments)
     (let* ((handle (database-handle database))
            (record
             (if raw
                 (wg-create-record handle size)
                 (wg-create-raw-record handle size))))
       (cond
         ((null-pointer-p record)
          (error 'database-error :db handle
                 :message "Couldn't create record"))
         (t
          (defrecord-init-slots
              (defrecord-kvargs arguments) handle record)
          (cons database record))))))

(defun parse-defrecord-name (name)
  (etypecase name
    (symbol
     (make-defrecord-name :name name))
    (cons
     (destructuring-bind (name &key extends indexed) name
       (make-defrecord-name :name name :extends extends :indexed indexed)))))

(defun make-defrecord-type (name)
  (let ((predicate (make-defrecord-func name :p)))
    `(progn
       (defun ,predicate (record)
         (destructuring-bind (record database) record
           (let ((type-field (wg-get-field database record 0)))
             (when (zerop type-field)
               (error 'database-error :db database
                      :message "Failed to fetch field"))
             (let ((field-type-encoded (wg-get-field-type database record 0)))
               (case type-field
                 (0 (error 'database-error :db database
                           :message "Failed to fetch field type"))
                 (5                         ; string
                  (let (type-field (decode-value type-field))
                    (string= type-field name))
                  (error 'database-error :db database
                         :message "Wrong record format")))))))
       (deftype ,name (&optional record)
         `(and (cons record) (satisfies ,predicate))))))

(defun defrecord-make-printer (name slots)
  `(defmethod print-object ((this name) stream)
     (iter
       (initially (format stream "#r(~:@(~A~)" ,name))
       (with slots := '(,@(mapcar #'defrecord-slot-name slots)))
       (with (record database) := this)
       (for slot :in slots)
       (for i :upfrom 1)
       (for field := (wg-get-field database record i))
       (format stream " :~:@(~A~) ~a" slot (decode-value field))
       (finally (format stream ")")))))

(defmacro define-record (name (&rest fields) &optional documentation)
  "This will create setter, getter and accessor functions
   for operations on records. Will also create a type predicate."
  (let* ((slots (parse-defrecord-slots fields))
         (name (parse-defrecord-name name))
         (type (make-defrecord-type (parse-defrecord-name name)))
         (constructor
          (make-defrecord-constructor
           (defrecord-name-name name) slots))
         (accessors (defrecord-make-acessors name slots))
         (printer (defrecord-make-printer name slots)))
    `(progn ,type ,constructor ,@accessors ,printer)))

(defmethod create-record ((this database) size &optional raw)
  (let ((record
         (if raw
             (wg-create-record (database-handle this) size)
             (wg-create-raw-record (database-handle this) size))))
    (if (null-pointer-p record)
        (error 'database-error :db (database-handle this)
               :message "Couldn't create record")
        record)))

(defmethod delete-record ((this database) record)
  (unless (zerop (wg-delete-record (database-handle this) record))
    (error 'database-error :db (database-handle this)
           :message "Failed to delete record")))

(defmethod first-record ((this database))
  (let ((result (wg-get-first-record (database-handle this))))
    (if (null-pointer-p result)
        (error 'database-error :db (database-handle this)
               :message "Failed to retrieve first record")
        result)))

(defmethod next-record ((this database) &key previous-record nothrow)
  (if previous-record
      (let ((result (wg-get-next-record (database-handle this) previous-record)))
        (if (null-pointer-p result)
            (if nothrow nothrow
                (error 'database-error :db (database-handle this)
                       :message "Failed to retrieve record"))
            result))
      (first-record this)))
