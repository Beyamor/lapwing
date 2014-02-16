(ns lapwing.player
  (:require [lapwing.entities :as entities]
            [lapwing.entities.collisions :as collision]
            [lapwing.entity :as entity]
            [lapwing.input :as input]
            [lapwing.entity.fsm :as fsm]
            [lonocloud.synthread :as ->]))

(defn try-grabbing
  [player solid solids]
  (let [top-difference (- (entity/top solid)
                          (entity/top player))]
    (-> player
      (->/when (and (>= top-difference -10)
                    (<= top-difference 5)
                    (not (collision/above? solid solids)))
               (assoc-in [:pos :y] (entity/top solid))
               (fsm/change-state :grabbing)))))

(fsm/def player
         falling
         (begin [player]
                (assoc player :gravity true))
         (update [player {:keys [entities input-state]}]
                 (let [falling-down?  (pos? (-> player :vel :y))
                       solids         (entities/filter entities :solid?)]
                   (-> player
                     (->/cond
                       (collision/below? player solids)
                       (fsm/change-state :walking)

                       (and falling-down?
                            (collision/left? player solids)
                            (input/is-down? input-state :move-left))
                       (try-grabbing (collision/left player solids) solids)

                       (and falling-down?
                            (collision/right? player solids)
                            (input/is-down? input-state :move-right))
                       (try-grabbing (collision/right player solids) solids)))))

         walking
         (begin [player]
                (-> player
                  (assoc :gravity false)
                  (assoc-in [:key-walker :can-walk?] true)
                  (assoc-in [:vel :y] 0)))
         (update [player {:keys [entities input-state]}]
                 (-> player
                   (->/cond
                     (input/was-pressed? input-state :jump)
                     (fsm/change-state :jumping)

                     (not (collision/below? player (entities/filter entities :solid?)))
                     (fsm/change-state :falling))))

         jumping
         (begin [{{:keys [initial-amount]} :player-jumper :as player}]
                (-> player
                  (update-in [:vel :y] - initial-amount)
                  (assoc-in [:key-walker :can-walk?] true)
                  (assoc-in [:player-jumper :additionals-applied] 0)))
         (update [{{:keys [additionals-applied number-of-additionals additional-amount]} :player-jumper
                   :as player}
                  {:keys [input-state]}]
                 (-> player
                   (->/if (or (>= additionals-applied number-of-additionals)
                              (input/was-released? input-state :jump))
                          (fsm/change-state :falling)
                          (->
                            (update-in [:vel :y] - additional-amount)
                            (update-in [:player-jumper :additionals-applied] inc)))))

         grabbing
         (begin [player]
                (-> player
                  (assoc :gravity false)
                  (assoc-in [:key-walker :can-walk?] false)
                  (->/in [:vel]
                         (assoc :x 0 :y 0))))
         (update [player {:keys [entities input-state]}]
                 (-> player
                   (->/when (input/was-pressed? input-state :jump)
                            (->/if (input/is-down? input-state :move-down)
                                   (fsm/change-state :falling)
                                   (fsm/change-state :jumping))))))
