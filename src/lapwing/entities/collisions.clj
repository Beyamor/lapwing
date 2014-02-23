(ns lapwing.entities.collisions
  (:require [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lonocloud.synthread :as ->]))

(defn- collides-with
  [e]
  (fn [other]
    (and (not (entity/= e other))
         (entity/collide? e other))))

(defn check
  [e es]
  (-> es
    (entities/in-entity-region e)
    (entities/any?
      (collides-with e))))

(defn all
  [e es]
  (-> es
    (entities/in-entity-region e)
    (entities/filter
      (collides-with e))
    entities/list))

(def ? check)

(defn below
  [e es]
  (? (update-in e [:pos :y] inc)
     es))

(def below? below)

(defn left
  [e es]
  (? (update-in e [:pos :x] dec)
     es))

(def left? left)

(defn right
  [e es]
  (? (update-in e [:pos :x] inc)
     es))

(def right? right)

(defn above
  [e es]
  (? (update-in e [:pos :y] dec)
     es))

(def above? above)
