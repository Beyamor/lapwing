(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util]
            [lonocloud.synthread :as ->])
  (:refer-clojure :exclude [filter remove]))

(def empty-entities
  {:entities  {}
   :grid      {}})

(declare add)

(defn create
  [initial-entities]
  (reduce add empty-entities initial-entities))

(defn select-ids
  [es ids]
  (-> es
    (update-in [:entities] select-keys ids)))

(defn filter
  [es pred?]
  (select-ids es
              (for [[id e] (:entities es)
                    :when (pred? e)]
                id)))

(defn those-with
  [es components]
  (filter es
          #(entity/has-components? % components)))

(defn any?
  [es pred?]
  (loop [es (-> es :entities vals)]
    (when (seq es)
      (if (pred? (first es))
        (first es)
        (recur (rest es))))))

(defn of-type
  [es type]
  (filter es #(= type (:type %))))

(defn each
  [es f]
  (for [[id e] (:entities es)]
    (f e)))

(def grid-size 200)

(defn grid-dims
  [e]
  [(-> e entity/left (/ grid-size) Math/floor)
   (-> e entity/right (/ grid-size) Math/ceil)
   (-> e entity/top (/ grid-size) Math/floor)
   (-> e entity/bottom (/ grid-size) Math/ceil)])

(defn grid-indices
  [e]
  (let [[left right top bottom] (grid-dims e)]
    (for [x (range left (inc right))
          y (range top (inc bottom))]
      [x y])))

(defn add-to-grid
  [grid e]
  (let [id (entity/id e)]
    (reduce
      (fn [grid [x y]]
        (update-in grid [x y] (fnil conj #{}) id))
      grid (grid-indices e))))

(defn remove-from-grid
  [grid e]
  (let [id (entity/id e)]
    (reduce
      (fn [grid [x y]]
        (update-in grid [x y] (fnil disj #{}) id))
      grid (grid-indices e))))

(defn update-only
  [es who updater]
  (let [id        (entity/id who)
        original  (get-in es [:entities id])
        updated   (updater original)]
    (-> es
      (assoc-in [:entities id] updated)
      (->/when (not= (:pos original) (:pos updated))
               (->/in [:grid]
                      (remove-from-grid original)
                      (add-to-grid updated))))))

(defn add
  [es e]
  (-> es
    (assoc-in [:entities (entity/id e)] e)
    (update-in [:grid] add-to-grid e)))

(defn remove
  [es e]
  (-> es
    (update-in [:entities] dissoc (entity/id e))))
