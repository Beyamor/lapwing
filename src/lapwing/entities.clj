(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util])
  (:refer-clojure :exclude [filter remove]))

(def empty-entities
  {:entities {}})

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

(defn update-only
  [es who updater]
  (let [id (entity/id who)]
    (update-in es [:entities id] updater)))

(defn add
  [es e]
  (-> es
    (assoc-in [:entities (entity/id e)] e)))

(defn remove
  [es e]
  (-> es
    (update-in [:entities] dissoc (entity/id e))))
