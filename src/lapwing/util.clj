(ns lapwing.util)

(def ids (atom 0))

(defn gen-id
  []
  (swap! ids inc))

(defn map-over-keys
  [f m]
  (into m
        (for [[k v] m]
          [k (f v)])))
