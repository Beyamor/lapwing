(ns lapwing.core
  (:require [lapwing.util :as util :refer [defs now]]
            [lapwing.image :as image]
            [lapwing.entities :as entities]
            [lapwing.entities.collisions :as collision]
            [lapwing.entity :as entity]
            [lapwing.entity.fsm :as fsm]
            [lapwing.input :as input]
            [lapwing.player :as player]
            [lapwing.game.entities :as game-entities]
            [lapwing.game.effectors :as game-effectors]
            [lapwing.game.systems :as game-systems]
            [lapwing.game.sections :as sections]
            [lapwing.cameras :as cam]
            [seesaw.core :as s]
            [seesaw.color :as s.col]
            [seesaw.timer :as s.time]
            [lonocloud.synthread :as ->])
  (:import java.awt.event.KeyEvent)
  (:gen-class :main true))

(set! *warn-on-reflection* true)

(defs
  window-width  800
  window-height 600)

(defn create-entities
  []
  (entities/create
    (concat
      [(entity/create game-entities/player)
       (entity/create game-entities/the-beast)]
      (for [wall (sections/template->entities
                   sections/first-section-template)]
        (entity/create wall)))))

(defn create-initial-game-state
  []
  {:entities      (create-entities)
   :camera        (cam/simple-camera window-width window-height)
   :time          (now)
   :last-section  0})

(defn create-canvas
  [[width height] render-state input-state]
  (let [set-key-state! (fn [state]
                         (fn [^KeyEvent e]
                           (input/set-state! input-state (.getKeyCode e) state)))
        ^java.awt.Component canvas (s/canvas 
                                     :size   [width :by height]
                                     :paint  (fn [c ^java.awt.Graphics2D g]
                                               (let [{:keys [entities time-delta camera]} @render-state]
                                                 (when entities
                                                   (doto g
                                                     (.setBackground (s.col/color "white"))
                                                     (.clearRect 0 0 width height))
                                                   (dorun
                                                     (-> entities
                                                       (entities/in-region
                                                         (cam/left camera) (cam/right camera)
                                                         (cam/top camera) (cam/bottom camera))
                                                       (entities/each
                                                         (fn [{:keys [pos debug-rect hitbox]}]
                                                           (when debug-rect
                                                             (doto g
                                                               (.setColor (s.col/color debug-rect))
                                                               (.fillRect (- (:x pos) (:x camera))
                                                                          (- (:y pos) (:y camera))
                                                                          (:width hitbox) (:height hitbox))))))))
                                                   (when time-delta
                                                     (doto g
                                                       (.setColor (s.col/color "blue"))
                                                       (.fillRect 0 0 20 20)
                                                       (.setColor (s.col/color "white"))
                                                       (.drawString (-> time-delta / int str) 3 15))))))
                                     :listen  [:key-pressed   (set-key-state! :down)
                                               :key-released  (set-key-state! :up)])]
    (.setFocusable canvas true)
    (s.time/timer
      (fn [_]
        (s/repaint! canvas))
      :delay 17)
    canvas))

(defn contains-end-statement?
  [statements]
  (some #(= :end (first %)) statements))

(defn effect-statements
  [game-state statements] 
  (reduce
    (fn [game-state [statement-type & data]]
      (if-let [effector (get game-effectors/all statement-type)]
        (merge game-state (apply effector game-state data))
        (throw (Exception. (str "No effector for " statement-type)))))
    game-state
    (filter identity statements)))

(defn run
  [render-state input-state]
  (loop [game-state (create-initial-game-state)]
    (let [start-time  (now)
          time-delta    (- start-time (:time game-state))
          game-state    (assoc game-state
                               :time start-time
                               :time-delta time-delta
                               :input-state (input/update! input-state))
          es          (:entities game-state)
          game-state  (reduce
                        (fn [game-state produce]
                          (let [statements (produce game-state)]
                            (if (contains-end-statement? statements)
                              (create-initial-game-state)
                              (effect-statements game-state statements))))
                        game-state
                        [game-systems/update-key-walkers
                         game-systems/update-key-shooters
                         game-systems/apply-gravity
                         game-systems/update-fsm
                         game-systems/move-dynamic-bodies
                         game-systems/move-camera
                         game-systems/extend-level
                         game-systems/pace-the-beast
                         game-systems/check-for-getting-eaten])]
      (send render-state (constantly game-state))
      ; eat up the remaning time
      (let [remaining-time (- 1/30
                              (- (now) start-time))]
        (when (pos? remaining-time)
          (Thread/sleep (* 1000 remaining-time))))
      (recur game-state))))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        input-state   (doto (input/create-state)
                        (input/def!
                          :jump       KeyEvent/VK_X
                          :shoot      KeyEvent/VK_C
                          :move-left  [KeyEvent/VK_KP_LEFT  KeyEvent/VK_LEFT]
                          :move-right [KeyEvent/VK_KP_RIGHT KeyEvent/VK_RIGHT]
                          :move-down  [KeyEvent/VK_KP_DOWN  KeyEvent/VK_DOWN]))
        canvas        (create-canvas [window-width window-height] render-state input-state)]
    (doto (Thread. #(run render-state input-state))
      .start)
    (s/invoke-later
      (-> (s/frame
            :title    "Lapwing"
            :content  canvas
            :on-close :exit)
        s/pack!
        s/show!))))
