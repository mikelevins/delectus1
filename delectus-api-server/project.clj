(defproject delectus-api-server "0.1.0-SNAPSHOT"
  :description "Delectus 2 API server"
  :url "http://delect.us"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.10.0"]
                 ;; Compojure - A basic routing library
                 [compojure "1.6.1"]
                 ;; Our Http library for client/server
                 [http-kit "2.3.0"]
                 ;; Ring defaults - for query params etc
                 [ring/ring-defaults "0.3.2"]
                 ;; Clojure data.JSON library
                 [org.clojure/data.json "0.2.6"]
                 ;; Couchbase Java client
                 [com.couchbase.client/java-client "2.7.9"]
                 ;; aero config library
                 [aero "1.1.3"]]

  :main ^:skip-aot delectus-api-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})