(ns lapwing.player
  (:require [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lapwing.input :as input]
            [lonocloud.synthread :as ->]))

(declare change-state)

(def states
  {:falling
   {:begin (fn [player]
             (assoc player :gravity true))

    :update (fn [player es input-state]
              (let [solids (entities/those-with es [:solid])
                    check  (update-in player [:pos :y] inc)]
                (-> player
                  (->/when (entities/any? solids #(entity/collide? check %))
                           (change-state :default)))))}

   :default
   {:begin   (fn [player]
               (-> player
                 (assoc :gravity false)
                 (assoc-in [:vel :y] 0)))

    :update  (fn [player es input-state]
               (-> player
                 (->/when (input/is-down? input-state :jump)
                          (change-state :jumping))))}

   :jumping
   {:begin   (fn [{:keys [jumper] :as player}]
               (assoc-in player [:vel :y] (* -1 jumper)))

    :update  (fn [player es input-state]
               (change-state player :falling))}})

(defn change-state
  [player new-state]
  (let [begin (get-in states [new-state :begin])]
    (-> player
      (assoc :player-state new-state)
      begin)))

(defn update-state
  [player entities input-state]
  (let [update (get-in states [(:player-state player) :update])]
    (update player entities input-state))) 
