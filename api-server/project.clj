(defproject delectus-api "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [
                 [aero "1.1.3"]                ; config library
                 [buddy/buddy-auth "2.2.0"]    ; auth
                 [buddy/buddy-core "1.6.0"] 
                 [buddy/buddy-hashers "1.4.0"] ; password-hashing
                 [buddy/buddy-sign "3.1.0"]
                 [clj-time "0.15.2"]    ; time utilities
                 [com.couchbase.client/java-client "2.7.9"] ; couchbase client
                 [hiccup "1.0.5"]       ; html generation
                 [metosin/compojure-api "2.0.0-alpha30"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/tools.logging "0.5.0"]
                 [ring "1.8.0"]           ; http server abstraction
                 [ring-cors "0.1.13"]     ; cors handling
                 [ring/ring-json "0.5.0"] ; json serialization
                 [tupelo "0.9.175"]       ; route debugging
                 ]
  :source-paths ["src/clj" "src/cljs" "src/cljc"]
  :ring {:handler delectus-api.routes/app
         :nrepl {:start? true :port 22022}}
  :uberjar-name "delectus-api.jar"
  :profiles {:dev {:dependencies [[javax.servlet/javax.servlet-api "3.1.0"]]
                   :plugins [[lein-ring "0.12.5"]]}})

