(ns lapwing.entity
  (:require [lapwing.util :as util]))

(defn create
  [& {:as components}]
  (assoc components
         ::id (util/gen-id)))

(defn id
  [thing]
  (if (number? thing)
    thing
    (::id thing)))

(defn has-components?
  [e components]
  (every? #(contains? e %) components))
