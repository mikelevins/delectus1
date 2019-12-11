(defproject delectus-api "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [metosin/compojure-api "2.0.0-alpha30"]
                 ;; config library
                 [aero "1.1.3"]
                 ;; auth library
                 [buddy/buddy-auth "2.2.0"]
                 ;; password-hashing library
                 [buddy/buddy-hashers "1.4.0"]
                 ;; Couchbase Java client
                 [com.couchbase.client/java-client "2.7.9"]
                 ;; http server abstraction
                 [ring "1.8.0"]
                 ;; time utilities
                 [tick "0.4.21-alpha"]]
  :ring {:handler delectus-api.handler/app}
  :uberjar-name "delectus-api.jar"
  :profiles {:dev {:dependencies [[javax.servlet/javax.servlet-api "3.1.0"]]
                   :plugins [[lein-ring "0.12.5"]]}})
