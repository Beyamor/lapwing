(ns lapwing.core
  (:require [lapwing.util :as util]
            [seesaw.core :as s]
            [seesaw.color :as s.col]
            [seesaw.timer :as s.time]
            [lonocloud.synthread :as ->]))

(def initial-player
  {:pos {:x 300 :y 300}})

(defn create-entities
  []
  (let [player-id (util/gen-id)]
    {player-id initial-player}))

(defn create-canvas
  [[width height] render-state]
  (let [canvas (s/canvas 
                 :size   [width :by height]
                 :paint  (fn [c g]
                           (let [render-state @render-state]
                             (when render-state
                               (doto g
                                 (.setBackground (s.col/color "white"))
                                 (.clearRect 0 0 width height)
                                 (.setColor (s.col/color "black")))
                               (doseq [[_ {:keys [pos]}] (:entities render-state)]
                                 (.fillRect g
                                            (:x pos) (:y pos)
                                            10 10))))))]
    (s.time/timer
      (fn [_]
        (s/repaint! canvas))
      :delay 17)
    canvas))

(defn update-positions
  [entities]
  (util/map-over-keys
    (fn [e]
      (update-in e [:pos :x] + 1))
    entities))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        canvas        (create-canvas [800 600] render-state)]
    (-> (fn []
          (loop [game-state {:entities (create-entities)}]
            (let [new-state (-> game-state
                              (->/in [:entities]
                                     update-positions))]
              (send render-state (constantly new-state))
              (Thread/sleep 20)
              (recur new-state))))
      Thread. .start)
    (s/invoke-later
      (-> (s/frame
            :title    "Lapwing"
            :content  canvas
            :on-close :exit)
        s/pack!
        s/show!))))
