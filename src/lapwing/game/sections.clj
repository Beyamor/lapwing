(ns lapwing.game.sections
  (:use [lapwing.util :only [indexed defs]])
  (:require [lapwing.game.entities :as game-entities]
            [lapwing.entity :as entity]))

(defs
  width         8
  height        8
  pixel-width   (* width game-entities/unit-width)
  pixel-height  (* height game-entities/unit-width))

(def xs-and-ys
  (for [x (range width)
        y (range height)]
    [x y]))

(defn vecs->template
  [vs]
  (into {}
        (for [[y row]     (indexed vs)
              [x symbol]  (indexed row)]
          [[x y] symbol])))

(def first-section-template
  (vecs->template
    [[:w :w :w :w :w :w :w :w]
     [:w :_ :_ :_ :_ :_ :_ :_]
     [:w :_ :_ :_ :_ :_ :_ :_]
     [:w :_ :_ :_ :_ :_ :_ :_]
     [:w :_ :_ :_ :_ :_ :_ :_]
     [:w :_ :_ :_ :_ :_ :_ :_]
     [:w :_ :_ :_ :_ :_ :_ :_]
     [:w :w :w :w :w :w :w :w]]))

(def all-templates
  (->>
    [
     [[:w :w :w :w :w :w :w :w]
      [:_ :w :w :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :g :_]
      [:_ :_ :_ :_ :_ :w :w :w]
      [:_ :_ :_ :g :_ :_ :_ :_]
      [:_ :_ :w :w :w :_ :_ :_]
      [:_ :_ :w :w :w :_ :g :_]
      [:w :w :w :w :w :w :w :w]]

     [[:w :w :w :w :w :w :w :w]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :w :w :_ :g :w :w :_]
      [:_ :w :w :_ :w :w :w :_]
      [:_ :w :w :g :_ :w :w :_]
      [:_ :w :w :w :_ :w :w :_]
      [:_ :_ :_ :_ :g :w :w :_]
      [:w :w :w :w :w :w :w :w]]

     [[:w :w :w :w :w :w :w :w]
      [:_ :_ :g :_ :g :_ :_ :_]
      [:_ :_ :w :w :w :w :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :g :_ :_ :_ :_]
      [:_ :_ :_ :w :w :_ :_ :_]
      [:_ :_ :g :w :w :g :_ :_]
      [:w :w :w :w :w :w :w :w]]

     [[:w :w :w :w :w :w :w :w]
      [:w :w :g :g :_ :_ :_ :_]
      [:w :w :w :w :w :w :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :g :w :_]
      [:_ :_ :_ :_ :_ :w :w :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:w :w :w :w :w :w :w :w]]
     ]

    (map vecs->template)))

(defn any-template
  []
  (rand-nth all-templates))

(defn grid->world-pos
  [x y]
  [(* x game-entities/unit-width)
   (* y game-entities/unit-width)])

(defn walls
  [template]
  (for [[x y :as xy]  xs-and-ys
        :let          [symbol (get template xy)]
        :when         (= symbol :w)
        :let          [[x y] (grid->world-pos x y)]]
    (game-entities/wall
      x y)))

(defn gems
  [template]
  (for [[x y :as xy]  xs-and-ys
        :let          [symbol (get template xy)]
        :when         (= symbol :g)
        :let          [[x y]  (grid->world-pos x y)
                       gem    (game-entities/gem x y)]]
        (-> gem
          (update-in [:pos :y] + (- game-entities/unit-width
                                    (entity/height gem))))))


(defn realize-template
  [template]
  (concat
    (walls template)
    (gems template)))
