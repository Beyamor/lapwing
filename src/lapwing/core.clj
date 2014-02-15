(ns lapwing.core
  (:require [lapwing.util :as util]
            [lapwing.image :as image]
            [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lapwing.input :as input]
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
         :pos
         {:x 300
          :y 300}
         :vel
         {:x 0
          :y 0}
         :key-walker
         {:speed 7}
         :debug-rect
         "red"
         :gravity
         true
         :hitbox
         {:width  48
          :height 48}
         :player-state
         :falling)]
      (for [x (range 0 800 48)]
        (entity/create
          :pos
          {:x x
           :y 500}
          :debug-rect
          "black"
          :hitbox
          {:width   48
           :height  48}
          :solid
          true)))))

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

(defn integrate-velocities
  [es]
  (-> es
    (entities/update-those-with
      [:pos :vel]
      (fn [{{vx :x vy :y} :vel :as e}]
        (-> e
          (update-in [:pos :x] + vx)
          (update-in [:pos :y] + vy))))))

(defn apply-gravity
  [es]
  (-> es
    (entities/update-those-with
      [:vel :gravity]
      (fn [{:keys [gravity] :as e}]
        (-> e
          (->/when gravity
                   (update-in [:vel :y] inc)))))))

(defn update-player-state
  [es]
  (-> es
    (entities/update-those-with
      [:player-state]
      (fn [e]
        (case (:player-state e)
          :falling
          (let [solids  (entities/those-with es [:solid])
                check-e (update-in e [:pos :y] inc)]
            (-> e
              (->/when (entities/any? solids #(entity/collide? check-e %))
                       (assoc :player-state :default)
                       (assoc :gravity false)
                       (assoc-in [:vel :y] 0))))

          e)))))

(defn run
  [render-state input-state]
  (loop [game-state {:entities (create-entities)}]
    (let [input-state @input-state
          new-state   (-> game-state
                        (->/in [:entities]
                               (updated-key-walkers input-state)
                               update-player-state
                               apply-gravity
                               integrate-velocities))]
      (send render-state (constantly new-state))
      (Thread/sleep 20)
      (recur new-state))))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        input-state   (doto (input/create-state)
                        (input/def!
                          :walk-left  [KeyEvent/VK_A KeyEvent/VK_KP_LEFT KeyEvent/VK_LEFT]
                          :walk-right [KeyEvent/VK_D KeyEvent/VK_KP_RIGHT KeyEvent/VK_RIGHT]))
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
