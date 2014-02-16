(ns lapwing.player
  (:require [lapwing.entities :as entities]
            [lapwing.entities.collisions :as collision]
            [lapwing.entity :as entity]
            [lapwing.input :as input]
            [lapwing.entity.fsm :as fsm]
            [lonocloud.synthread :as ->]))

(defn grab
  [player wall]
  (-> player
    (assoc-in [:pos :y] (entity/top wall))
    (fsm/change-state :grabbing)))

(fsm/def player
         falling
         (begin [player]
                (assoc player :gravity true))
         (update [player es input-state]
                 (let [walls (entities/of-type es :wall)]
                   (-> player
                     (->/cond
                       (collision/below? player walls)
                       (fsm/change-state :walking)

                       (and (collision/left? player walls)
                            (input/is-down? input-state :move-left))
                       (grab (collision/left player walls))

                       (and (collision/right? player walls)
                            (input/is-down? input-state :move-right))
                       (grab (collision/right player walls))))))

         walking
         (begin [player]
                (-> player
                  (assoc :gravity false)
                  (assoc-in [:key-walker :can-walk] true)
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
                  (assoc-in [:key-walker :can-walk] true)
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
                  (assoc :gravity false)
                  (assoc-in [:key-walker :can-walk] false)
                  (->/in [:vel]
                         (assoc :x 0 :y 0))))
         (update [player es input-state]
                 (-> player
                   (->/when (input/was-pressed? input-state :jump)
                            (->/if (input/is-down? input-state :move-down)
                                   (fsm/change-state :falling)
                                   (fsm/change-state :jumping))))))
