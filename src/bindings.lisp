;; -*- mode: lisp -*-
(in-package :cl-whitedb)

(define-foreign-library libwgdb
  (:unix (:or "/usr/local/lib/libwgdb.so.0.0.0"
              "/usr/local/lib/libwgdb.so.0.0"
              "/usr/local/lib/libwgdb.so.0"
              "/usr/local/lib/libwgdb.so"))
  (t (:default "libwgdb")))

(use-foreign-library libwgdb)

(defctype wg-int :int)            ; ptrdiff_t
(defctype wg-uint :uint)              ; size_t

(defcstruct wg-query-arg
  (column wg-int)
  (cond wg-int)
  (value wg-int))

(defcstruct wg-query
  (qtype wg-int)
  (arglist (:pointer (:struct wg-query-arg)))
  (argc wg-int)
  (columnt wg-int)
  (curr-offset wg-int)
  (end-offset wg-int)
  (curr-slot wg-int)
  (end-slot wg-int)
  (curr-record wg-int)
  (mpool :pointer)
  (curr-page :pointer)
  (curr-pidx wg-int)
  (res-count wg-uint))

(defcfun "wg_attach_database" :pointer
  "returns a pointer to the database, NULL if failure"
  (dbasename :string)
  (size wg-int))

(defcfun "wg_attach_existing_database" :pointer
  "like wg_attach_database, but does not create a new base"
  (dbasename :string))

(defcfun "wg_attach_logged_database" :pointer
  "like wg_attach_database, but activates journal logging on creation"
  (dbasename :string)
  (size wg-int))

(defcfun "wg_detach_database" :int
  "detaches a database: returns 0 if OK"
  (database :pointer))

(defcfun "wg_delete_database" :int
  "deletes a database: returns 0 if OK"
  (dbasename :string))

(defcfun "wg_attach_local_database" :pointer
  (size wg-int))

(defcfun "wg_delete_local_database" :void
  (database :pointer))

(defcfun "wg_database_freesize" wg-int
  (database :pointer))

(defcfun "wg_database_size" wg-int
  (database :pointer))

(defcfun "wg_create_record" :pointer
  "returns NULL when error, ptr to rec otherwise"
  (database :pointer)
  (length wg-int))

(defcfun "wg_create_raw_record" :pointer
  "returns NULL when error, ptr to rec otherwise"
  (database :pointer)
  (length wg-int))

(defcfun "wg_delete_record" wg-int
  "returns 0 on success, non-0 on error"
  (database :pointer)
  (record :pointer))

(defcfun "wg_get_first_record" :pointer
  "returns NULL when error or no recs"
  (database :pointer))

(defcfun "wg_get_next_record" :pointer
  "returns NULL when error or no more recs"
  (database :pointer)
  (record :pointer))

(defcfun "wg_get_first_parent" :pointer
  (database :pointer)
  (record :pointer))

(defcfun "wg_get_next_parent" :pointer
  (database :pointer)
  (record :pointer)
  (parent :pointer))

(defcfun "wg_get_record_len" wg-int
  "returns negative int when error"
  (database :pointer)
  (record :pointer))

(defcfun "wg_get_record_dataarray" (:pointer wg-int)
  "returns negative int when error"
  (database :pointer)
  (record :pointer))

(defcfun "wg_set_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data wg-int))

(defcfun "wg_set_new_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data wg-int))

(defcfun "wg_set_int_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data wg-int))

(defcfun "wg_set_double_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data :double))

(defcfun "wg_set_str_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data :string))

(defcfun "wg_update_atomic_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data wg-int)
  (old-data wg-int))

(defcfun "wg_set_atomic_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data wg-int))

(defcfun "wg_add_int_atomic_field" wg-int
  "returns negative int when err, 0 when ok"
  (database :pointer)
  (record :pointer)
  (field-num wg-int)
  (data wg-int))

(defcfun "wg_get_field" wg-int
  "returns 0 when error"
  (database :pointer)
  (record :pointer)
  (field-num wg-int))

(defcfun "wg_get_field_type" wg-int
  "returns 0 when error"
  (database :pointer)
  (record :pointer)
  (field-num wg-int))

(defcfun "wg_get_encoded_type" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_free_encoded" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_null" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_null" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_int" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_int" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_double" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_double" :double
  (database :pointer)
  (data :double))

(defcfun "wg_encode_fixpoint" wg-int
  (database :pointer)
  (data :double))

(defcfun "wg_decode_fixpoint" :double
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_date" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_date" :int
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_time" wg-int
  (database :pointer)
  (data :int))

(defcfun "wg_decode_time" :int
  (database :pointer)
  (data wg-int))

(defcfun "wg_current_utcdate" :int (database :pointer))

(defcfun "wg_current_localdate" :int (database :pointer))

(defcfun "wg_current_utctime" :int (database :pointer))

(defcfun "wg_current_localtime" :int (database :pointer))

(defcfun "wg_strf_iso_datetime" :int
  (database :pointer)
  (date :int)
  (time :int)
  (buffer :string))

(defcfun "wg_strp_iso_date" :int
  (database :pointer)
  (buffer :string))

(defcfun "wg_strp_iso_time" :int
  (database :pointer)
  (buffer :string))

(defcfun "wg_ymd_to_date" :int
  (database :pointer)
  (year :int)
  (month :int)
  (day :int))

(defcfun "wg_hms_to_time" :int
  (database :pointer)
  (hour :int)
  (minute :int)
  (second :int)
  (prt :int))                           ; what is prt?

(defcfun "wg_date_to_ymd" :void
  (database :pointer)
  (date :int)
  (year (:pointer :int))
  (month (:pointer :int))
  (day (:pointer :int)))

(defcfun "wg_time_to_hms" :void
  (database :pointer)
  (time :int)
  (hour (:pointer :int))
  (minute (:pointer :int))
  (second (:pointer :int))
  (prt (:pointer :int)))

(defcfun "wg_encode_str" wg-int
  "let lang==NULL if not used"
  (database :pointer)
  (string :string)
  (language :string))

(defcfun "wg_decode_str" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_str_lang" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_str_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_str_lang_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_str_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_decode_str_lang_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_encode_xmlliteral" wg-int
  "xmlliteral (standard C string: zero-terminated array of chars)
   along with obligatory attached xsd:type str"
  (database :pointer)
  (string :string)
  (xsdtype :string))

(defcfun "wg_encode_xmlliteral" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_xmlliteral_xsdtype" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_xmlliteral_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_xmlliteral_xsdtype_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_xmlliteral_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_decode_xmlliteral_xsdtype_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_encode_uri" wg-int
  "uri (standard C string: zero-terminated array of chars)
   along with an optional namespace str
   let nspace==NULL if not used"
  (database :pointer)
  (uri :string)
  (namespace :string))

(defcfun "wg_decode_uri" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_uri_prefix" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_uri_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_uri_prefix_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_uri_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_decode_uri_prefix_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_encode_blob" wg-int
  "blob (binary large object, i.e. any kind of data)
   along with an obligatory length in bytes"
  (database :pointer)
  (string :string)
  (type :string)
  (length wg-int))

(defcfun "wg_decode_blob" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_blob_type" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_blob_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_blob_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_decode_blob_type_len" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_decode_blob_type_copy" wg-int
  (database :pointer)
  (data wg-int)
  (buffer :string)
  (buffer-length wg-int))

(defcfun "wg_encode_record" wg-int
  (database :pointer)
  (data :pointer))

(defcfun "wg_decode_record" :pointer
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_char" wg-int
  (database :pointer)
  (data :char))

(defcfun "wg_decode_char" :char
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_anonconst" wg-int
  (database :pointer)
  (data :string))

(defcfun "wg_decode_anonconst" :string
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_var" wg-int
  (database :pointer)
  (var-num wg-int))

(defcfun "wg_decode_var" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_dump" wg-int
  "dump shared memory database to the disk"
  (database :pointer)
  (filename :string))

(defcfun "wg_import_dump" wg-int
  "import database from the disk"
  (database :pointer)
  (filename :string))

(defcfun "wg_start_logging" wg-int
  "activate journal logging globally"
  (database :pointer))

(defcfun "wg_stop_logging" wg-int
  "deactivate journal logging"
  (database :pointer))

(defcfun "wg_replay_log" wg-int
  "restore from journal"
  (database :pointer)
  (filename :string))

(defcfun "wg_start_write" wg-int
  "start write transaction"
  (database :pointer))

(defcfun "wg_end_write" wg-int
  "end write transaction"
  (database :pointer)
  (lock wg-int))

(defcfun "wg_start_read" wg-int
  "end write transaction"
  (database :pointer))

(defcfun "wg_end_read" wg-int
  "end read transaction"
  (database :pointer)
  (lock wg-int))

(defcfun "wg_print_db" :void
  (database :pointer))

(defcfun "wg_print_record" :void
  (database :pointer)
  (record (:pointer wg-int)))

(defcfun "wg_snprint_value" :void
  (database :pointer)
  (enc wg-int)
  (buffer :string)
  (buffer-length :int))

(defcfun "wg_parse_and_encode" wg-int
  (database :pointer)
  (buffer :string))

(defcfun "wg_parse_and_encode_param" wg-int
  (database :pointer)
  (buffer :string))

(defcfun "wg_export_db_csv" :void
  (database :pointer)
  (filename :string))

(defcfun "wg_import_db_csv" wg-int
  (database :pointer)
  (filename :string))

(defcfun "wg_make_query" (:pointer (:struct wg-query))
  (database :pointer)
  (match-record :pointer)
  (record-length wg-int)
  (arglist (:pointer (:struct wg-query-arg))))

(defcfun "wg_make_query_rc" (:pointer (:struct wg-query))
  (database :pointer)
  (match-record :pointer)
  (record-length wg-int)
  (arglist (:pointer (:struct wg-query-arg)))
  (row-limit wg-uint))

(defcfun "wg_fetch" :pointer
  (database :pointer)
  (query (:pointer (:struct wg-query))))

(defcfun "wg_free_query" :void
  (database :pointer)
  (query (:pointer (:struct wg-query))))

(defcfun "wg_encode_query_param_null" wg-int
  (database :pointer)
  (data :string))

(defcfun "wg_encode_query_param_recor" wg-int
  (database :pointer)
  (data :string))

(defcfun "wg_encode_query_param_char" wg-int
  (database :pointer)
  (data :char))

(defcfun "wg_encode_query_param_fixpoint" wg-int
  (database :pointer)
  (data :double))

(defcfun "wg_encode_query_param_date" wg-int
  (database :pointer)
  (data :int))

(defcfun "wg_encode_query_param_time" wg-int
  (database :pointer)
  (data :int))

(defcfun "wg_encode_query_param_var" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_query_param_int" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_encode_query_param_double" wg-int
  (database :pointer)
  (data :double))

(defcfun "wg_encode_query_param_str" wg-int
  (database :pointer)
  (data :string)
  (language :string))

(defcfun "wg_encode_query_param_xmlliteral" wg-int
  (database :pointer)
  (data :string)
  (xsdtype :string))

(defcfun "wg_encode_query_param_uri" wg-int
  (database :pointer)
  (data :string)
  (prefix :string))

(defcfun "wg_free_query_param" wg-int
  (database :pointer)
  (data wg-int))

(defcfun "wg_find_record" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :pointer)
  (last-record :pointer))

(defcfun "wg_find_record_null" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :string)
  (last-record :pointer))

(defcfun "wg_find_record_record" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :pointer)
  (last-record :pointer))

(defcfun "wg_find_record_char" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :char)
  (last-record :pointer))

(defcfun "wg_find_record_fixpoint" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :double)
  (last-record :pointer))

(defcfun "wg_find_record_date" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :int)
  (last-record :pointer))

(defcfun "wg_find_record_time" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :int)
  (last-record :pointer))

(defcfun "wg_find_record_var" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data wg-int)
  (last-record :pointer))

(defcfun "wg_find_record_int" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :int)
  (last-record :pointer))

(defcfun "wg_find_record_double" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :double)
  (last-record :pointer))

(defcfun "wg_find_record_str" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :string)
  (last-record :pointer))

(defcfun "wg_find_record_xmlliteral" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :string)
  (xsdtype :string)
  (last-record :pointer))

(defcfun "wg_find_record_uri" :pointer
  (database :pointer)
  (field-num wg-int)
  (cond wg-int)
  (data :string)
  (prefix :string)
  (last-record :pointer))

(defcfun "wg_register_external_db" wg-int
  (database :pointer)
  (external-database :pointer))

(defcfun "wg_encode_external_data" wg-int
  (database :pointer)
  (external-database :pointer)
  (encoded wg-int))

(defcfun "wg_parse_json_file" wg-int
  (database :pointer)
  (file-name :string))

(defcfun "wg_check_json" wg-int
  (database :pointer)
  (buffer :string))

(defcfun "wg_parse_json_document" wg-int
  (database :pointer)
  (buffer :string)
  (document (:pointer :pointer)))

(defcfun "wg_parse_json_fragment" wg-int
  (database :pointer)
  (buffer :string)
  (document (:pointer :pointer)))

