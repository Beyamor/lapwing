(ns lapwing.entities.collisions
  (:require [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lonocloud.synthread :as ->]))

(defn check
  [e es]
  (entities/any?
    es
    #(and (not (entity/= e %))
          (entity/collide? e %))))

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
