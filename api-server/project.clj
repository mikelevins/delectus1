(defproject delectus-api-server "0.1.0-SNAPSHOT"
  :description "Delectus 2 API server"
  :url "http://delect.us"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.10.0"]
                 ;; standard Clojure JSON library
                 [org.clojure/data.json "0.2.6"]
                 ;; standard Clojure CSV library
                 [org.clojure/data.csv "0.1.4"]
                 ;; config library
                 [aero "1.1.3"]
                 ;; auth library
                 [buddy/buddy-auth "2.2.0"]
                 ;; password-hashing library
                 [buddy/buddy-hashers "1.4.0"]
                 ;; response-signing library
                 [buddy/buddy-sign "3.1.0"]
                 ;; Compojure - A basic routing library
                 [compojure "1.6.1"]
                 ;; compojure-api - creating swagger-documented api routes
                 [metosin/compojure-api "2.0.0-alpha30"]
                 ;; Couchbase Java client
                 [com.couchbase.client/java-client "2.7.9"]
                 ;; Http library
                 [http-kit "2.3.0"]
                 [ring "1.8.0"]
                 [ring/ring-anti-forgery "1.3.0"]
                 [ring-cors "0.1.13"]
                 [ring/ring-defaults "0.3.2"]
                 ;; hiccup HTML generator
                 [hiccup "1.0.5"]
                 ;; time utilities
                 [tick "0.4.21-alpha"]]

  ;; for skipping AOT in Pedestal apps
  ;; :main ^:skip-aot delectus-api-server.core
  :main delectus-api-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
