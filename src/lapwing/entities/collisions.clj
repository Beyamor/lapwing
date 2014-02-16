(ns lapwing.entities.collisions
  (:require [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lonocloud.synthread :as ->]))

(defn ?
  [e es]
  (entities/any?
    es
    #(and (not (entity/= e %))
          (entity/collide? e %))))

(defn below?
  [e es]
  (? (update-in e [:pos :y] inc)
     es))
