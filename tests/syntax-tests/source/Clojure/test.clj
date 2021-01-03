(ns clojure-sample.core
    (:gen-class))
  
  (require '[clj-time.core :as t])
  (require '[clj-time.format :as f])
  
  ;; Product record
  (defrecord Product [id name available price])
  
  ;; Positional constructor
  (def product1 (->Product "1" "T-Shirt 1" true 15.00))
  
  ;; Map constructor
  (def product2 (map->Product
                 {:id "2"
                  :name "T-Shirt 2"
                  :available true
                  :price 20.00}))
  
  ;; Nested
  (def product3 {:id "1"
                 :name "Product 1"
                 :available true
                 :sellers [{:id "1"
                            :name "Seller 1"
                            :stock 3},
                           {:id 2
                            :name "Seller 2"
                            :stock 5}]})
  
  ;; Set
  (def categories #{"shirts" "shoes" "belts"})
  
  ;; List
  (def wishlist '(1 2))
  
  ;; Recursion
  (defn factorial [value] (cond
                            (<= value 1) 1
                            :else (* value (factorial (- value 1)))))
  
  (def basic-formatter (f/formatter "YYYY-MM-dd hh:mm:ss"))
  (defn now [] (f/unparse basic-formatter (t/now)))
  (defn log
    ([] (println (now) "No message"))
    ([message] (println (now)  message)))
  
  (defn -main
    [& args]
    (println (:id product1))
    (println (:name product2))
    (println (:name (get (:sellers product3) 0)))
    (println (first categories))
    (println wishlist)
    (println (factorial 5))
    (log)
    (log "Message"))
  
