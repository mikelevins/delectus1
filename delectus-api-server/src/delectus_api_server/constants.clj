(ns delectus-api-server.constants)

;;; ---------------------------------------------------------------------
;;; bucket names
;;; ---------------------------------------------------------------------

(def +scratch-bucket-name+ "scratch")
(def +delectus-users-bucket-name+ "delectus_users")
(def +delectus-content-bucket-name+ "delectus_content")

;;; ---------------------------------------------------------------------
;;; document types
;;; ---------------------------------------------------------------------

(def +collection-type+ "delectus_collection")
(def +list-type+ "delectus_list")
(def +user-type+ "delectus_user")

;;; ---------------------------------------------------------------------
;;; JSON object attributes
;;; ---------------------------------------------------------------------

(def +columns-attribute+ "columns")
(def +deleted-attribute+ "deleted")
(def +email-attribute+ "email")
(def +enabled-attribute+ "enabled")
(def +id-attribute+ "id")
(def +items-attribute+ "items")
(def +lists-attribute+ "lists")
(def +name-attribute+ "name")
(def +password-hash-attribute+ "password-hash")
(def +owner-id-attribute+ "owner-id")
(def +type-attribute+ "type")
