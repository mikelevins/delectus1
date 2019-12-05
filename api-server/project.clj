(defproject delectus-api-server "0.1.0-SNAPSHOT"
  :description "Delectus 2 API server"
  :url "http://delect.us"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.10.0"]
                 ;; Compojure - A basic routing library
                 [compojure "1.6.1"]
                 ;; Http library
                 [http-kit "2.3.0"]
                 ;; Ring defaults - for query params etc
                 [ring/ring-defaults "0.3.2"]
                 ;; Ring cors - to enable cross-origin requests
                 [ring-cors "0.1.13"]
                 ;; standard Clojure JSON library
                 [org.clojure/data.json "0.2.6"]
                 ;; standard Clojure CSV library
                 [org.clojure/data.csv "0.1.4"]
                 ;; Couchbase Java client
                 [com.couchbase.client/java-client "2.7.9"]
                 ;; config library
                 [aero "1.1.3"]
                 ;; hiccup HTML generator
                 [hiccup "1.0.5"]
                 ;; auth library
                 [buddy/buddy-auth "2.2.0"]
                 ;; password-hashing library
                 [buddy/buddy-hashers "1.4.0"]
                 ;; response-signing library
                 [buddy/buddy-sign "3.1.0"]
                 ;; time utilities
                 [tick "0.4.21-alpha"]]

  ;; for skipping AOT in Pedestal apps
  ;; :main ^:skip-aot delectus-api-server.core
  :main delectus-api-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
