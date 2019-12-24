(ns delectus-api.schema
  (:require
   [delectus-api.constants :refer :all]
   [schema.core :as s]
   ))

;;; ---------------------------------------------------------------------
;;; schemata
;;; ---------------------------------------------------------------------

(s/defschema AuthenticationRequest
  {:userid s/Str
   :password s/Str})

(s/defschema CollectionMap
  {s/Str s/Any})

(s/defschema CollectionRenameRequest
  {:userid s/Str
   :collectionid s/Str
   :newname s/Str})

(s/defschema DeleteCollectionRequest
  {:userid s/Str
   :collectionid s/Str})

(s/defschema DeleteListRequest
  {:userid s/Str
   :listid s/Str})

(s/defschema ListMakeUncollectedRequest
  {:userid s/Str
   :listid s/Str})

(s/defschema ListMap
  {s/Str s/Any})

(s/defschema ListMoveToCollectionRequest
  {:userid s/Str
   :listid s/Str
   :collectionid s/Str})

(s/defschema ListRenameRequest
  {:userid s/Str
   :listid s/Str
   :newname s/Str})

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

(s/defschema NewCollectionRequest
  {:userid s/Str
   :name s/Str})

(s/defschema NewListRequest
  {:userid s/Str
   :name s/Str})

(s/defschema UserData
  {(s/required-key +id-attribute+) s/Str
   (s/required-key +name-attribute+) s/Str
   (s/required-key +email-attribute+) (s/maybe s/Str)
   (s/required-key +password-hash-attribute+) (s/maybe s/Str)
   (s/required-key +type-attribute+) s/Str
   (s/required-key +enabled-attribute+) s/Bool})


