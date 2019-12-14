(ns delectus-api.schema
  (:require
   [schema.core :as s]
   ))

;;; ---------------------------------------------------------------------
;;; schemata
;;; ---------------------------------------------------------------------

(s/defschema AuthenticationRequest
  {:userid s/Str
   :password s/Str})

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

(s/defschema UserData
  {:userid s/Str
   :email s/Str
   :name s/Str})

(s/defschema CollectionRenameRequest
  {:userid s/Str
   :collectionid s/Str
   :newname s/Str})

(s/defschema NewCollectionRequest
  {:userid s/Str
   :name s/Str})


