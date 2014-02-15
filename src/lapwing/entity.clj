(ns lapwing.entity
  (:require [lapwing.util :as util]
            [lonocloud.synthread :as ->]))

(defn create
  [& {:as components}]
  (assoc components
         ::id (util/gen-id)))

(defn id
  [thing]
  (if (number? thing)
    thing
    (::id thing)))

(defn has-component?
  [e component]
  (contains? e component))

(defn has-components?
  [e components]
  (every? #(has-component? e %) components))

(defn left
  [e]
  (-> 0
    (->/when (has-component? e :pos)
             (+ (-> e :pos :x)))))

(defn right
  [e]
  (-> e
    left
    (->/when (has-component? e :hitbox)
             (+ (-> e :hitbox :width)))))

(defn top
  [e]
  (-> 0
    (->/when (has-component? e :pos)
             (+ (-> e :pos :y)))))

(defn bottom
  [e]
  (-> e
    top
    (->/when (has-component? e :hitbox)
             (+ (-> e :hitbox :height)))))

(defn collide?
  [e1 e2]
  (not (or
         (< (right e2) (left e1))
         (> (left e2) (right e1))
         (< (bottom e2) (top e1))
         (> (top e2) (bottom e1)))))
