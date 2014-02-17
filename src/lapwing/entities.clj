(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util])
  (:refer-clojure :exclude [filter remove]))

(defn create
  [initial-entities]
  (into {}
        (for [e initial-entities]
          [(entity/id e) e])))

(defn filter
  [es pred?]
  (select-keys es
               (for [[id e] es
                     :when (pred? e)]
                 id)))

(defn those-with
  [es components]
  (filter es
          #(entity/has-components? % components)))

(defn update-those-with
  [es components f]
  (->>
    (those-with es components)
    (util/map-over-keys f)
    (merge es)))

(defn any?
  [es pred?]
  (loop [es (vals es)]
    (when (seq es)
      (if (pred? (first es))
        (first es)
        (recur (rest es))))))

(defn of-type
  [es type]
  (filter es #(= type (:type %))))

(defn each
  [es f]
  (for [[id e] es]
    (f e)))

(defn update-only
  [es who updater]
  (let [id (entity/id who)]
    (update-in es [id] updater)))

(defn add
  [es e]
  (assoc es (entity/id e) e))

(defn remove
  [es e]
  (dissoc es (entity/id e)))
