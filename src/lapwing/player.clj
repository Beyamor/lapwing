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
    (when (and (>= top-difference -10)
               (<= top-difference 5)
               (not (collision/above? solid solids)))
      (concat
        [[:set player [:pos :y] (entity/top solid)]]
        (fsm/change-state player :grabbing)))))

(fsm/def player
         falling
         (begin [player]
                [[:set player [:gravity] true]])
         (update [player {:keys [entities input-state]}]
                 (let [falling-down?  (pos? (-> player :vel :y))
                       solids         (entities/filter entities :solid?)]
                   (cond
                     (collision/below? player solids)
                     (fsm/change-state player :walking)

                     (and falling-down?
                          (collision/left? player solids)
                          (input/is-down? input-state :move-left))
                     (try-grabbing player (collision/left player solids) solids)

                     (and falling-down?
                          (collision/right? player solids)
                          (input/is-down? input-state :move-right))
                     (try-grabbing player (collision/right player solids) solids))))

         walking
         (begin [player]
                [[:set player [:gravity] false
                  [:key-walker :can-walk?] true
                  [:vel :y] 0]])
         (update [player {:keys [entities input-state]}]
                 (cond
                   (input/was-pressed? input-state :jump)
                   (fsm/change-state player :jumping)

                   (not (collision/below? player (entities/filter entities :solid?)))
                   (fsm/change-state player :falling)))

         jumping
         (begin [{{:keys [initial-amount]} :player-jumper :as player}]
                [[:update player [:vel :y] #(- % initial-amount)]
                 [:set player [:key-walker :can-walk?] true
                  [:player-jumper :additionals-applied] 0]])
         (update [{{:keys [additionals-applied number-of-additionals additional-amount]} :player-jumper
                   :as player}
                  {:keys [input-state]}]
                 (if (or (>= additionals-applied number-of-additionals)
                         (input/was-released? input-state :jump))
                   (fsm/change-state player :falling)
                   [[:update player [:vel :y] #(- % additional-amount)
                     [:player-jumper :additionals-applied] inc]]))

         grabbing
         (begin [player]
                [[:set player [:gravity] false
                  [:key-walker :can-walk?] false
                  [:vel :x] 0
                  [:vel :y] 0]])
         (update [player {:keys [entities input-state]}]
                 (when (input/was-pressed? input-state :jump)
                   (if (input/is-down? input-state :move-down)
                     (fsm/change-state player :falling)
                     (fsm/change-state player :jumping)))))
