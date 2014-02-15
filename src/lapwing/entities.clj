(ns lapwing.entities
  (:require [lapwing.entity :as entity]
            [lapwing.util :as util]))

(defn create
  [initial-entities]
  (into {}
        (for [e initial-entities]
          [(entity/id e) e])))

(defn update-those-with
  [es components f]
  (util/map-over-keys
    (fn [e]
      (if (entity/has-components? e components)
        (f e)
        e))
    es))
