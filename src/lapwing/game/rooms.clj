(ns lapwing.game.rooms
  (:require [lapwing.util :as util :refer [indexed defs]]
            [lapwing.game.entities :as game-entities]
            [lapwing.entity :as entity]
            [lonocloud.synthread :as ->]))


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

(def first-top-template
  (vecs->template
    [[:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :w :w :w :w :w :w :w]]))

(def first-bottom-template
  (vecs->template
    [[:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :_ :_ :_ :_ :_ :_ :_]
     [:W :W :W :W :W :W :W :W]]))

(def bottom-templates
  (->>
    [
     [[:_ :w :w :_ :_ :_ :_ :_]
      [:_ :w :w :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :w :w :w]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :w :w :w :_ :_ :_]
      [:_ :_ :w :w :w :_ :_ :_]
      [:W :W :W :W :W :W :W :W]]

     [[:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :w :w :_ :_ :w :w :_]
      [:_ :w :w :_ :w :w :w :_]
      [:_ :w :w :_ :_ :w :w :_]
      [:_ :w :w :w :_ :w :w :_]
      [:_ :_ :_ :_ :_ :w :w :_]
      [:W :W :W :W :W :W :W :W]]

     [[:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :w :w :w :w :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :w :w :_ :_ :_]
      [:_ :_ :_ :w :w :_ :_ :_]
      [:W :W :W :W :W :W :W :W]]

     [[:w :w :_ :_ :_ :_ :_ :_]
      [:w :w :_ :_ :_ :_ :_ :_]
      [:w :w :w :w :w :w :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :w :_]
      [:_ :_ :_ :_ :_ :w :w :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:W :W :W :W :W :W :W :W]]
     ]

    (map vecs->template)))

(def top-templates
  (->>
    [
     [[:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :w :w :_]
      [:_ :_ :_ :_ :_ :w :w :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :w :w :_ :_ :_ :_]
      [:_ :_ :w :w :_ :_ :_ :_]
      [:w :w :w :w :w :w :w :w]]

     [[:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :w :w :w :w]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:_ :_ :_ :_ :_ :_ :_ :_]
      [:w :w :w :_ :_ :_ :_ :_]]

     [[:_ :w :w :w :_ :_ :_ :_]
      [:_ :_ :_ :w :_ :_ :_ :_]
      [:w :_ :_ :w :_ :w :w :w]
      [:_ :_ :_ :w :_ :_ :_ :w]
      [:_ :_ :_ :w :_ :_ :_ :w]
      [:_ :_ :w :w :_ :_ :_ :w]
      [:_ :_ :_ :_ :_ :_ :_ :w]
      [:w :w :w :w :w :w :_ :w]]
     ]

    (map vecs->template)))

(defn any-bottom
  []
  (rand-nth bottom-templates))

(defn any-top
  []
  (rand-nth top-templates))

(defn grid->world-pos
  [x y]
  [(* x game-entities/unit-width)
   (* y game-entities/unit-width)])

(defs
  is-wall?  #{:w :W}
  is-empty? #{:_})

(defn walls
  [template]
  (for [[x y :as xy]  xs-and-ys
        :when         (is-wall? (get template xy))
        :let          [[x y] (grid->world-pos x y)]]
    (->
      (game-entities/wall x y)
      (->/when (= (get template xy) :W)
               (assoc :destructible? false)))))

(defn gem-position-weights
  [template]
  (into {}
        (for [[x y :as xy]  xs-and-ys
              :when         (and (is-wall? (get template xy))
                                 (> y 0))
              :let          [y (dec y)]
              :when         (is-empty? (get template [x y]))] 
          [[x y] 1])))

(defn random-gem-position
  [position-weights]
  (->
    (for [[position weight] position-weights]
      (repeat weight position))
    util/flatten-1
    rand-nth))

(let [desired-value 100]
  (defn gems
    [template]
    (loop [position-weights (gem-position-weights template), total-value 0, gems []]
      (if (or (>= total-value desired-value)
              (empty? position-weights))
        gems
        (let [[x y :as xy] (random-gem-position position-weights)
              [x y] (grid->world-pos x y)
              gem   (game-entities/gem x y)]
          (recur
            (dissoc position-weights xy)
            (+ total-value (-> gem :gem :value))
            (conj gems gem)))))))

(defn realize-template
  [template]
  (concat
    (walls template)
    (gems template)))
