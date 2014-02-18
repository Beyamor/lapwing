(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util]
            [lonocloud.synthread :as ->]
            clojure.set)
  (:refer-clojure :exclude [filter list]))

;
;           The entity collection protocol
;
(defprotocol EntityCollection
  (-get-entity [es id])
  (-get-ids [es])
  (-select-ids [es ids])
  (-add-entity [es e])
  (-remove-entity [es id])
  (-list [es])
  (-update-only [es id updater]))

(defn get-entity [es e-or-id]
  (-get-entity es (entity/id e-or-id)))

(defn get-ids [es]
  (set (-get-ids es)))

(defn select-ids [es ids]
  (-select-ids es ids))

(defn add-entity [es e]
  (-add-entity es e))

(defn remove-entity [es e-or-id]
  (-remove-entity es (entity/id e-or-id)))

(defn list [es]
  (-list es))

(defn update-only [es who updater]
  (-update-only es (entity/id who) updater))

;
;           General collection stuff
;
(defn filter
  [es pred?]
  (select-ids es
              (for [e (list es)
                    :when (pred? e)]
                (entity/id e))))

(defn those-with
  [es components]
  (filter es
          #(entity/has-components? % components)))

(defn any?
  [es pred?]
  (loop [es (list es)]
    (when (seq es)
      (if (pred? (first es))
        (first es)
        (recur (rest es))))))

(defn of-type
  [es type]
  (filter es #(= type (:type %))))

(defn each
  [es f]
  (for [e (list es)]
    (f e)))

;
;           Maps are the simplest collections, yo
;
(extend-protocol EntityCollection
  clojure.lang.APersistentMap
  (-get-entity [this id]
    (get this (entity/id id)))

  (-get-ids [this]
    (-> this keys set))

  (-select-ids [this ids]
    (select-keys this ids))

  (-add-entity [this e]
    (assoc this (entity/id e) e))

  (-remove-entity [this id]
    (dissoc this id))

  (-list [this]
    (vals this))

  (-update-only [this id updater]
    (update-in this [id] updater)))

;
;           Spatial entity collections
;
(declare empty-spatial-entity-collection)

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

(defn add-entity-to-grid
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
    (-select-ids (:entities es))))

(defrecord SpatialEntityCollection
  [entities grid]

  EntityCollection
  (-get-entity [this id]
    (-> this :entities (get-entity id)))

  (-get-ids [this]
    (-> this :entities get-ids))

  (-select-ids [this ids]
    (if (> (count ids) (/ (count (:entities this)) 2))
      ; -remove-entity the unselected ids
      (let [ids-to-remove (clojure.set/difference (get-ids this) ids)]
        (reduce
          (fn [this id]
            (remove-entity this id))
          this ids-to-remove))
      ; otherwise, just rebuild from scratch
      (reduce add-entity empty-spatial-entity-collection
              (map #(get-entity this %) ids))))

  (-add-entity [this e]
    (-> this
      (update-in [:entities] add-entity e)
      (update-in [:grid] add-entity-to-grid e)))

  (-remove-entity [this id]
    (let [e (-get-entity this id)]
      (-> this
        (update-in [:entities] remove-entity e)
        (update-in [:grid] remove-from-grid e))))

  (-list [this]
    (-> this :entities list))

  (-update-only [this id updater]
    (let [original  (get-entity this id)
          updated   (updater original)]
      (-> this
        (->/in [:entities]
               (remove-entity id)
               (add-entity updated))
        (->/when (not= (:pos original) (:pos updated))
                 (->/in [:grid]
                        (remove-from-grid original)
                        (add-entity-to-grid updated)))))))

(def empty-spatial-entity-collection
  (->SpatialEntityCollection
    {} {}))

(defn create-spatial-collection
  [initial-entities]
  (reduce add-entity empty-spatial-entity-collection initial-entities))
