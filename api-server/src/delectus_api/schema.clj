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

(s/defschema CollectionDescription
  {(s/required-key "name") s/Str
   (s/required-key "id") s/Str
   (s/required-key "deleted") s/Bool})

(s/defschema CollectionRenameRequest
  {:userid s/Str
   :collectionid s/Str
   :newname s/Str})

(s/defschema DeleteCollectionRequest
  {:userid s/Str
   :collectionid s/Str})

(s/defschema ListMakeUncollectedRequest
  {:userid s/Str
   :listid s/Str})

(s/defschema ListMoveToCollectionRequest
  {:userid s/Str
   :listid s/Str
   :collectionid s/Str})

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

(s/defschema NewCollectionRequest
  {:userid s/Str
   :name s/Str})

(s/defschema UserData
  {:userid s/Str
   :email s/Str
   :name s/Str})


