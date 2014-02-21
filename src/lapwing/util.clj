(ns lapwing.util
  (:require [clojure.walk :as w]))

(def ids (atom 0))

(defn gen-id
  []
  (swap! ids inc))

(defn map-over-keys
  [f m]
  (into m
        (for [[k v] m]
          [k (f v)])))

(defn flatten-1
  [s]
  (apply concat s))

(defn direction->int
  [direction]
  (case direction
    :left -1
    :right 1))

(defmacro defs
  [& defs]
  `(do ~@(for [[name value] (partition 2 defs)]
           `(def ~name ~value))))
