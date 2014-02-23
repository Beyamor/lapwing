(ns lapwing.entity
  (:require [lapwing.util :as util]
            [lonocloud.synthread :as ->])
  (:refer-clojure :exclude [=]))

(defn create
  [components]
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

(defn width
  [e]
  (-> 0
    (->/when (has-component? e :hitbox)
             (+ (-> e :hitbox :width)))))

(defn height
  [e]
  (-> 0
    (->/when (has-component? e :hitbox)
             (+ (-> e :hitbox :height)))))

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
             (+ (width e))
             (- 0.0001))))

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
             (+ (height e))
             (- 0.0001))))

(defn collide?
  [e1 e2]
  (not (or
         (< (right e2) (left e1))
         (> (left e2) (right e1))
         (< (bottom e2) (top e1))
         (> (top e2) (bottom e1)))))

(defn =
  [e1 e2]
  (clojure.core/= (id e1) (id e2))) 
