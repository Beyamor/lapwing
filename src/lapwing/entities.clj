(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util]
            [lonocloud.synthread :as ->]
            clojure.set)
  (:refer-clojure :exclude [filter list get remove]))

;
;           Core protocols (+ wrappers)
;
(defprotocol EntityCollection
  (-get [es id])
  (-get-ids [es])
  (-select-ids [es ids])
  (-filter [es pred?])
  (-add [es e])
  (-remove [es id])
  (-list [es])
  (-update-only [es id updater]))

(defn get [es e-or-id]
  (-get es (entity/id e-or-id)))

(defn get-ids [es]
  (set (-get-ids es)))

(defn select-ids [es ids]
  (-select-ids es ids))

(defn filter [es pred?]
  (-filter es pred?))

(defn add [es e]
  (-add es e))

(defn remove [es e-or-id]
  (-remove es (entity/id e-or-id)))

(defn list [es]
  (-list es))

(defn update-only [es who updater]
  (-update-only es (entity/id who) updater))

(defprotocol SpatialAccess
  (-in-region [es left right top bottom]))

(extend-protocol SpatialAccess
  Object
  (-in-region [es left right top bottom]
    (filter es
            #(not (or (< right (entity/left %))
                      (> left (entity/right %))
                      (< bottom (entity/top %))
                      (> top (entity/bottom %)))))))

(defn in-region
  [es left right top bottom]
  (-in-region es left right top bottom))

(defn in-entity-region
  [es e]
  (in-region es
             (entity/left e) (entity/right e) (entity/top e) (entity/bottom e)))

;
;           General collection stuff
;

(declare new-delayed-filter-collection)
(declare empty-spatial-entity-collection)
(defn create
  ([initial-entities]
   (create (new-delayed-filter-collection) initial-entities))
   ;(create empty-spatial-entity-collection initial-entities))
   ;(create {} initial-entities))
  ([seed initial-entities]
   (reduce add seed initial-entities)))

(defn those-with
  [es components]
  (filter es
          #(entity/has-components? % components)))

(defn of-type
  [es type]
  (filter es
          #(= type (:type %))))

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

(defn get-first
  [es]
  (-> es list first))

;
;           Maps are the simplest collections, yo
;
(extend-protocol EntityCollection
  clojure.lang.APersistentMap
  (-get [this id]
    (clojure.core/get this (entity/id id)))

  (-get-ids [this]
    (-> this keys set))

  (-select-ids [this ids]
    (select-keys this ids))

  (-filter [this pred?] 
    (into {}
          (for [[id e] this
                :when (pred? e)]
            [id e])))

  (-add [this e]
    (assoc this (entity/id e) e))

  (-remove [this id]
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

(def grid-indices-
  (memoize
    (fn [left right top bottom]
      (let [xs (range left (inc right))
            ys (range top (inc bottom))]
        (for [x xs
              y ys]
          [x y])))))

(defn grid-indices
  [left right top bottom]
  (let [[left right top bottom] (grid-dims left right top bottom)]
    (grid-indices- left right top bottom)))

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

(defn ids-in-region
  [es left right top bottom]
  (let [grid (:grid es)]
    (->>
      (grid-indices left right top bottom)
      (map (fn [[x y]]
             (get-in grid [x y])))
      (reduce into #{}))))

(defrecord SpatialEntityCollection
  [entities grid]

  EntityCollection
  (-get [this id]
    (-get entities id))

  (-get-ids [this]
    (-get-ids entities))

  (-select-ids [this ids]
    (-select-ids entities ids))

  (-filter [this pred?]
    (-filter entities pred?))

  (-add [this e]
    (->SpatialEntityCollection
      (-add entities e)
      (add-to-grid grid e)))

  (-remove [this id]
    (let [e (-get entities id)]
      (->SpatialEntityCollection
        (-remove entities id)
        (remove-from-grid grid e))))

  (-list [this]
    (-list entities))

  (-update-only [this id updater]
    (let [original  (-get entities id)
          updated   (updater original)]
      (->SpatialEntityCollection
        (-> entities
          (-remove id)
          (-add updated))
        (-> grid
          (->/when (not= (:pos original) (:pos updated))
                   (remove-from-grid original)
                   (add-to-grid updated))))))

  SpatialAccess
  (-in-region [this left right top bottom]
    (-select-ids entities
                 (ids-in-region this left right top bottom))))

(def empty-spatial-entity-collection
  (->SpatialEntityCollection
    {} {}))

;
;         Delayed filter collections
;         (allowing us to compose filters)
;
(declare ->DelayedFilterCollection)
(defn new-delayed-filter-collection
  ([]
   (new-delayed-filter-collection
     empty-spatial-entity-collection))
  ([base-entities]
   (->DelayedFilterCollection
     base-entities
     (delay base-entities)
     [])))

(defn delay-filtering-entities
  [entities pred?s]
  (delay
    (filter entities
            (fn [e]
              (every? #(% e) pred?s)))))

(deftype DelayedFilterCollection
  [base-entities filtered-entities pred?s]

  EntityCollection
  (-get [this id]
    (-get @filtered-entities id))

  (-get-ids [this]
    (-get-ids @filtered-entities))

  (-select-ids [this ids]
    (-select-ids @filtered-entities ids))

  (-filter [this pred?]
    (let [pred?s (conj pred?s pred?)]
      (->DelayedFilterCollection
        base-entities
        (delay-filtering-entities base-entities pred?s)
        pred?s)))

  (-add [this e]
    (new-delayed-filter-collection
      (-add @filtered-entities e)))

  (-remove [this id]
    (new-delayed-filter-collection
      (-remove @filtered-entities id)))

  (-list [this]
    (-list @filtered-entities))

  (-update-only [this id updater]
    (new-delayed-filter-collection
      (-update-only @filtered-entities id updater)))

  SpatialAccess
  (-in-region [this left right top bottom]
    (let [regional-subset (-in-region base-entities
                                      left right top bottom)]
      (->DelayedFilterCollection 
        regional-subset
        (delay-filtering-entities regional-subset pred?s)
        pred?s))))
