(ns delectus-api-server.route-handlers)

;;; ---------------------------------------------------------------------
;;; generic test handlers
;;; ---------------------------------------------------------------------

(defn landing-page [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "<h1>Delectus 2 API Server, v 0.1</h1>"})

(defn hello-name [req]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (let [nm (:name (:params req))]
            (if nm
              (str "Hello, " nm "!")
              (str "Hello!")))})
