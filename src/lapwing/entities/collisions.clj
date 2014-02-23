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

(defn below
  [e es]
  (check (update-in e [:pos :y] inc)
     es))

(defn left
  [e es]
  (check (update-in e [:pos :x] dec)
     es))

(defn right
  [e es]
  (check (update-in e [:pos :x] inc)
     es))

(defn above
  [e es]
  (check (update-in e [:pos :y] dec)
     es))
