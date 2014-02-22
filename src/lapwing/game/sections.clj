(ns lapwing.game.sections
  (:use [lapwing.util :only [indexed defs]])
  (:require [lapwing.game.entities :as game-entities]))

(defs
  width         8
  height        8
  pixel-width   (* width game-entities/unit-width)
  pixel-height  (* height game-entities/unit-width))

(def first-section-template
  [[:w :w :w :w :w :w :w :w]
   [:w :_ :_ :_ :_ :_ :_ :_]
   [:w :_ :_ :_ :_ :_ :_ :_]
   [:w :_ :_ :_ :_ :_ :_ :_]
   [:w :_ :_ :_ :_ :_ :_ :_]
   [:w :_ :_ :_ :_ :_ :_ :_]
   [:w :_ :_ :_ :_ :_ :_ :_]
   [:w :w :w :w :w :w :w :w]])

(def all-templates
  [
   [[:w :w :w :w :w :w :w :w]
    [:_ :w :w :_ :_ :_ :_ :_]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:_ :_ :_ :_ :_ :w :w :w]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:_ :_ :w :w :w :_ :_ :_]
    [:_ :_ :w :w :w :_ :_ :_]
    [:w :w :w :w :w :w :w :w]]

   [[:w :w :w :w :w :w :w :w]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:_ :w :w :_ :_ :w :w :_]
    [:_ :w :w :_ :w :w :w :_]
    [:_ :w :w :_ :_ :w :w :_]
    [:_ :w :w :w :_ :w :w :_]
    [:_ :_ :_ :_ :_ :w :w :_]
    [:w :w :w :w :w :w :w :w]]

   [[:w :w :w :w :w :w :w :w]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:_ :_ :w :w :w :w :_ :_]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:_ :_ :_ :w :w :_ :_ :_]
    [:_ :_ :_ :w :w :_ :_ :_]
    [:w :w :w :w :w :w :w :w]]

   [[:w :w :w :w :w :w :w :w]
    [:w :w :_ :_ :_ :_ :_ :_]
    [:w :w :w :w :w :w :_ :_]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:_ :_ :_ :_ :_ :_ :w :_]
    [:_ :_ :_ :_ :_ :w :w :_]
    [:_ :_ :_ :_ :_ :_ :_ :_]
    [:w :w :w :w :w :w :w :w]]
   ])

(defn any-template
  []
  (rand-nth all-templates))

(defn template->entities
  [template]
  (for [[y row]     (indexed template)
        [x symbol]  (indexed row)
        :when       (not= :_ symbol)]
    (case symbol
      :w (game-entities/wall
           (* x game-entities/unit-width)
           (* y game-entities/unit-width)))))
