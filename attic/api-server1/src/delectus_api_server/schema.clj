(ns delectus-api-server.schema
  (:require
   [schema.core :as s]
   ))


(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

(s/defschema LoginResponse
  {:status s/Int
   :headers s/Str
   :body s/Str})
