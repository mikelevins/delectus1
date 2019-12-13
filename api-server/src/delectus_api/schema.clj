(ns delectus-api.schema
  (:require
   [schema.core :as s]
   ))

;;; ---------------------------------------------------------------------
;;; schemata
;;; ---------------------------------------------------------------------

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

(s/defschema UserData
  {:id s/Str
   :email s/Str
   :name s/Str})
