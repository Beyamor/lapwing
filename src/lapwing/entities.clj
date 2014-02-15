(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util]))

(defn create
  [initial-entities]
  (into {}
        (for [e initial-entities]
          [(entity/id e) e])))

(defn those-with
  [es components]
  (select-keys es
               (for [[id e] es
                     :when (entity/has-components? e components)]
                 id)))

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
