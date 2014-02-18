(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util]
            [lonocloud.synthread :as ->]
            clojure.set)
  (:refer-clojure :exclude [filter remove]))

(def empty-entities
  {:entities  {}
   :grid      {}})

(declare add remove)

(defn create
  [initial-entities]
  (reduce add empty-entities initial-entities))

(defn get-ids
  [es]
  (-> es :entities keys set))

(defn select-ids
  [es ids]
  ; if selecting many ids
  (if (> (count ids) (/ (count (:entities es)) 2))
    ; remove the unselected ids
    (let [ids-to-remove (clojure.set/difference (get-ids es) ids)]
      (reduce
        (fn [es id]
          (remove es id))
        es ids-to-remove))
    ; otherwise, just rebuild from scratch
    (do
    (reduce add empty-entities
            (map #(get-in es [:entities %]) ids)))))

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

(def grid-size 50)

(defn grid-dims
  [left right top bottom]
  [(-> left (/ grid-size) Math/floor)
   (-> right (/ grid-size) Math/ceil)
   (-> top (/ grid-size) Math/floor)
   (-> bottom (/ grid-size) Math/ceil)])

(defn grid-indices
  [left right top bottom]
  (let [[left right top bottom] (grid-dims left right top bottom)]
    (for [x (range left (inc right))
          y (range top (inc bottom))]
      [x y])))

(defn entity-grid-indices
  [e]
  (grid-indices
    (entity/left e)
    (entity/right e)
    (entity/top e)
    (entity/bottom e)))

(defn add-to-grid
  [grid e]
  (let [id (entity/id e)]
    (reduce
      (fn [grid [x y]]
        (update-in grid [x y] (fnil conj #{}) id))
      grid (entity-grid-indices e))))

(defn remove-from-grid
  [grid e]
  (let [id (entity/id e)]
    (reduce
      (fn [grid [x y]]
        (update-in grid [x y] (fnil disj #{}) id))
      grid (entity-grid-indices e))))

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
  (let [e (get-in es [:entities (entity/id e)])]
    (-> es
      (update-in [:entities] dissoc (entity/id e))
      (->/in [:grid]
             (remove-from-grid e)))))

(defn ids-in-region
  [es left right top bottom]
  (->>
    (grid-indices left right top bottom)
    (map (fn [[x y]]
           (get-in es [:grid x y])))
    (reduce into #{})))

(defn in-region
  [es left right top bottom]
  (->>
    (ids-in-region es left right top bottom)
    (select-ids es)))
