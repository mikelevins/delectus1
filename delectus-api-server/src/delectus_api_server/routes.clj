(ns delectus-api-server.routes
  (:require
   [compojure.core :refer :all]
   [compojure.route :as route]
   [delectus-api-server.handlers :as handlers])
  (:gen-class))

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes

  ;; general test routes
  ;; -------------------
  (GET "/echo" [] handlers/echo)
  (GET "/status" [] handlers/status)

  ;; -------------------
  ;; Delectus API routes
  ;; -------------------

  ;; admin
  ;; -------------------  
  
  ;; (GET "/delectus/register" [] handlers/register)               ; api/register-user
  ;; (GET "/delectus/registerupdate_user" [] handlers/update-user) ; api/update-user!

  ;; sessions
  ;; -------------------  

  (GET "/delectus/login" [] handlers/login)    ; api/email->user
  (GET "/delectus/userid" [] handlers/userid)  ; api/email->userid

  ;; collections
  ;; -------------------  

  (GET "/delectus/collections" [] handlers/collections)                       ; api/list-collections
  (GET "/delectus/new_collection" [] handlers/new-collection)                 ; api/create-collection
  (GET "/delectus/collection_named" [] handlers/collection-named)             ; api/find-collection-by-name
  (GET "/delectus/collection_with_id" [] handlers/collection-with-id)         ; api/find-collection-by-id
  (GET "/delectus/collection_add_list" [] handlers/collection-add-list)       ; api/collection-add-list
  (GET "/delectus/collection_remove_list" [] handlers/collection-remove-list) ; api/collection-remove-list
  ;; (GET "/delectus/delete_collection" [] handlers/delete-collection         ; api/mark-collection-deleted
  ;; (GET "/delectus/undelete_collection" [] handlers/undelete-collection     ; api/mark-collection-deleted
  ;; (GET "/delectus/collection_name" [] handlers/collection-name             ; api/get-collection-name
  ;; (GET "/delectus/rename_collection" [] handlers/rename-collection         ; api/update-collection-name
  ;; (GET "/delectus/collection_lists" [] handlers/collection-lists           ; api/get-collection-lists

  ;; lists
  ;; -------------------  

  (GET "/delectus/lists" [] handlers/lists)                                    ; api/list-lists
  (GET "/delectus/list_named" [] handlers/list-named)                          ; api/find-list-by-name
  (GET "/delectus/list_with_id" [] handlers/list-with-id)                      ; api/find-list-by-id
  ;; (GET "/delectus/new_list" [] handlers/new-list)                           ; api/create-list
  ;; (GET "/delectus/delete_list" [] handlers/delete-list)                     ; api/mark-list-deleted
  ;; (GET "/delectus/undelete_list" [] handlers/undelete-list)                 ; api/mark-list-deleted
  ;; (GET "/delectus/list_name" [] handlers/list-name                          ; api/list-name
  ;; (GET "/delectus/rename_list" [] handlers/rename-list                      ; api/update-list-name
  ;; (GET "/delectus/list_columns" [] handlers/list-columns                    ; api/list-columns
  ;; (GET "/delectus/column_named" [] handlers/column-named                    ; api/find-column-by-name
  ;; (GET "/delectus/column_with_id" [] handlers/column-with-id                ; api/find-column-by-id
  ;; (GET "/delectus/new_column" [] handlers/new-column                        ; api/list-add-column
  ;; (GET "/delectus/delete_column" [] handlers/delete-column                  ; api/mark-column-deleted
  ;; (GET "/delectus/undelete_column" [] handlers/undelete-column              ; api/mark-column-deleted
  ;; (GET "/delectus/column_name" [] handlers/column-name                      ; api/column-name
  ;; (GET "/delectus/rename_column" [] handlers/rename-column                  ; api/update-column-name
  ;; (GET "/delectus/list_items" [] handlers/list_items                        ; api/list-items
  ;; (GET "/delectus/item_with_id" [] handlers/item-with-id                    ; api/find-item-by-id
  ;; (GET "/delectus/new_item" [] handlers/new-item                            ; api/list-add-item
  ;; (GET "/delectus/delete_item" [] handlers/delete-item                      ; api/mark-item-deleted
  ;; (GET "/delectus/undelete_item" [] handlers/undelete-item                  ; api/mark-item-deleted
  ;; (GET "/delectus/item_column_value" [] handlers/item-column-value          ; api/item-column-value
  ;; (GET "/delectus/set_item_column_value" [] handlers/set-item-column-value  ; api/update-item-column-value
  
  ;; default ("Page not found") route
  ;; --------------------------------
  (route/not-found "Error, page not found!"))
