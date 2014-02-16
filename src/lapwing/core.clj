(ns lapwing.core
  (:require [lapwing.util :as util]
            [lapwing.image :as image]
            [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lapwing.entity.fsm :as fsm]
            [lapwing.input :as input]
            [lapwing.player :as player]
            [seesaw.core :as s]
            [seesaw.color :as s.col]
            [seesaw.timer :as s.time]
            [lonocloud.synthread :as ->])
  (:import java.awt.event.KeyEvent))

(defn create-entities
  []
  (entities/create
    (concat
      [(entity/create
         :pos {:x 300
               :y 300}
         :vel {:x 0
               :y 0}
         :key-walker {:speed 7}
         :debug-rect "red"
         :gravity true
         :hitbox {:width  48
                  :height 48}
         :solid true
         :state-machine {:name   :player
                         :state  :falling}
         :player-jumper {:initial-amount         10
                         :additional-amount      0.5
                         :number-of-additionals  5})]
      (for [x (range 0 800 48)]
        (entity/create
          :pos {:x x
                :y 500}
          :debug-rect "black"
          :hitbox {:width   48
                   :height  48}
          :solid true)))))

(defn create-canvas
  [[width height] render-state input-state]
  (let [set-key-state! (fn [state]
                         (fn [e]
                           (input/set-state! input-state (.getKeyCode e) state)))
        canvas (s/canvas 
                 :size   [width :by height]
                 :paint  (fn [c g]
                           (let [render-state @render-state]
                             (when render-state
                               (doto g
                                 (.setBackground (s.col/color "white"))
                                 (.clearRect 0 0 width height))
                               (doseq [[_ {:keys [pos debug-rect hitbox]}] (:entities render-state)
                                       :when debug-rect]
                                 (doto g
                                   (.setColor (s.col/color debug-rect))
                                   (.fillRect (:x pos) (:y pos)
                                              (:width hitbox) (:height hitbox)))))))
                 :listen  [:key-pressed   (set-key-state! :down)
                           :key-released  (set-key-state! :up)])]
    (.setFocusable canvas true)
    (s.time/timer
      (fn [_]
        (s/repaint! canvas))
      :delay 17)
    canvas))

(defn updated-key-walkers
  [es input-state]
  (let [dx (+ (if (input/is-down? input-state :walk-left)
                -1 0)
              (if (input/is-down? input-state :walk-right)
                1 0))]
    (-> es
      (entities/update-those-with
        [:key-walker :vel]
        (fn [{{:keys [speed] :or {speed 1}} :key-walker :as e}]
          (-> e
            (assoc-in [:vel :x] (* speed dx))))))))

(defn collides-with-other-entity?
  [e es]
  (entities/any? es
                 #(and (not (entity/= e %))
                       (entity/collide? e %))))

(defn move
  [es]
  (let [solids (entities/those-with es [:pos :solid])]
    (-> es
      (entities/update-those-with
        [:pos :vel]
        (fn [{{vx :x vy :y} :vel :as e}]
          (let [x-dir (if (pos? vx) inc dec)
                y-dir (if (pos? vy) inc dec)]
            (loop [x-step (Math/floor (Math/abs vx)), y-step (Math/floor (Math/abs vy)), e e]
              (if (or (pos? x-step) (pos? y-step))
                (let [e-          (update-in e [:pos :x] x-dir)
                      [e x-step]  (if (and (pos? x-step)
                                           (not (collides-with-other-entity? e- solids)))
                                    [e- (dec x-step)]
                                    [e 0])
                      e-          (update-in e [:pos :y] y-dir)
                      [e y-step]  (if (and (pos? y-step)
                                           (not (collides-with-other-entity? e- solids)))
                                    [e- (dec y-step)]
                                    [e 0])]
                  (recur x-step y-step e))
                e))))))))

(defn apply-gravity
  [es]
  (-> es
    (entities/update-those-with
      [:vel :gravity]
      (fn [{:keys [gravity] :as e}]
        (-> e
          (->/when gravity
                   (update-in [:vel :y] inc)))))))

(defn update-fsm
  [es input-state]
  (-> es
    (entities/update-those-with
      [:state-machine]
      (fn [player]
        (fsm/update player es input-state)))))

(defn run
  [render-state input-state]
  (loop [game-state {:entities (create-entities)}]
    (let [input-state (input/update! input-state)
          new-state   (-> game-state
                        (->/in [:entities]
                               (updated-key-walkers input-state)
                               (update-fsm input-state)
                               apply-gravity
                               move))]
      (send render-state (constantly new-state))
      (Thread/sleep 20)
      (recur new-state))))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        input-state   (doto (input/create-state)
                        (input/def!
                          :jump       KeyEvent/VK_X
                          :walk-left  [KeyEvent/VK_KP_LEFT  KeyEvent/VK_LEFT]
                          :walk-right [KeyEvent/VK_KP_RIGHT KeyEvent/VK_RIGHT]))
        canvas        (create-canvas [800 600] render-state input-state)]
    (doto (Thread. #(run render-state input-state))
      .start)
    (s/invoke-later
      (-> (s/frame
            :title    "Lapwing"
            :content  canvas
            :on-close :exit)
        s/pack!
        s/show!))))
