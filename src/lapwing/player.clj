(ns lapwing.player
  (:require [lapwing.entities :as entities]
            [lapwing.entities.collisions :as collision]
            [lapwing.entity :as entity]
            [lapwing.input :as input]
            [lapwing.entity.fsm :as fsm]
            [lonocloud.synthread :as ->]))

(fsm/def player
         falling
         (begin [player]
                (assoc player :gravity true))
         (update [player es input-state]
                 (let [walls (entities/of-type es :wall)]
                   (-> player
                     (->/cond
                       (collision/below? player walls)
                       (fsm/change-state :walking)))))

         walking
         (begin [player]
                (-> player
                  (assoc :gravity false)
                  (assoc-in [:vel :y] 0)))
         (update [player es input-state]
                 (-> player
                   (->/cond
                     (input/was-pressed? input-state :jump)
                     (fsm/change-state :jumping)

                     (not (collision/below? player (entities/of-type es :wall)))
                     (fsm/change-state :falling))))

         jumping
         (begin [{{:keys [initial-amount]} :player-jumper :as player}]
                (-> player
                  (update-in [:vel :y] - initial-amount)
                  (assoc-in [:player-jumper :additionals-applied] 0)))
         (update [{{:keys [additionals-applied number-of-additionals additional-amount]} :player-jumper
                   :as player}
                  es input-state]
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
                  (->/in [:vel]
                         (assoc :x 0 :y 0))))
         (update [player es input-state]
                 player))
