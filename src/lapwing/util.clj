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

(def direction->int
  {:left  -1
   :right 1})

(def direction->angle
  {:right 0
   :left  (- Math/PI)
   :up    (* -0.5 Math/PI)
   :down  (* 0.5 Math/PI)})

(defmacro defs
  [& defs]
  `(do ~@(for [[name value] (partition 2 defs)]
           `(def ~name ~value))))

(defn indexed
  [s]
  (map-indexed
    (fn [index value]
      [index value])
    s))

(defn now
  []
  (/ (System/nanoTime) 1000000000))

(def mmerge (partial merge-with merge))
