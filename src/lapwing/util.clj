(ns lapwing.util)

(def ids (atom 0))

(defn gen-id
  []
  (swap! ids inc))
