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

(def +delectus-collection-document-type+ "delectus_collection")
(def +delectus-list-document-type+ "delectus_list")
(def +delectus-user-document-type+ "delectus_user")

;;; ---------------------------------------------------------------------
;;; JSON object attributes
;;; ---------------------------------------------------------------------

(def +type-attribute+ "type")
(def +id-attribute+ "id")
(def +name-attribute+ "name")
(def +owner-id-attribute+ "owner-id")
(def +items-attribute+ "items")
