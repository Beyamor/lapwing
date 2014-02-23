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

(defn x
  [e]
  (-> 0
    (->/when (has-component? e :pos)
             (+ (-> e :pos :x)))))

(defn x=
  [e new-x]
  (assoc-in e [:pos :x] new-x))

(defn y
  [e]
  (-> 0
    (->/when (has-component? e :pos)
             (+ (-> e :pos :y)))))

(defn y=
  [e new-y]
  (assoc-in e [:pos :y] new-y))

(defn width
  [e]
  (-> 0
    (->/when (has-component? e :hitbox)
             (+ (-> e :hitbox :width)))))

(defn half-with
  [e]
  (/ (width e) 2))

(defn height
  [e]
  (-> 0
    (->/when (has-component? e :hitbox)
             (+ (-> e :hitbox :height)))))

(defn half-height
  [e]
  (/ (height e) 2))

(defn left
  [e]
  (x e))

(def left= x=)

(defn right
  [e]
  (-> e
    left
    (->/when (has-component? e :hitbox)
             (+ (width e))
             (- 0.0001))))

(defn top
  [e]
  (y e))

(def top= y=)

(defn bottom
  [e]
  (-> e
    top
    (->/when (has-component? e :hitbox)
             (+ (height e))
             (- 0.0001))))

(defn center-x
  [e]
  (+ (left e) (half-with e)))

(defn center-x=
  [e new-center-x]
  (left= e
      (- new-center-x (half-with e))))

(defn center-y
  [e]
  (+ (top e) (half-height e)))

(defn center-y=
  [e new-center-y]
  (top= e
        (- new-center-y (half-height e))))

(defn center=
  [e new-center-x new-center-y]
  (-> e
    (center-x= new-center-x)
    (center-y= new-center-y)))

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
