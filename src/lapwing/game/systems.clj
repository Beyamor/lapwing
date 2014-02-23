(ns lapwing.game.systems
  (:require [lapwing.util :as util]
            [lapwing.entities :as entities]
            [lapwing.entities.collisions :as collision]
            [lapwing.entity :as entity]
            [lapwing.entity.fsm :as fsm]
            [lapwing.input :as input]
            [lapwing.game.entities :as game-entities]
            [lapwing.game.sections :as sections]
            [lapwing.cameras :as cam]
            [lonocloud.synthread :as ->]))

(defn update-key-walkers
  [{:keys [entities input-state]}]
  (let [dx (+ (if (input/is-down? input-state :move-left)
                -1 0)
              (if (input/is-down? input-state :move-right)
                1 0))
        direction (if (neg? dx) :left :right)]
    (util/flatten-1
      (-> entities
        (entities/those-with [:key-walker :vel])
        (entities/filter #(-> % :key-walker :can-walk?))
        (entities/each
          (fn [{{:keys [speed]} :key-walker :as e}]
            (concat
              [[:accelerate e {:x (* speed dx)
                               :relative? false}]
               (when (and (not (zero? dx))
                          (entity/has-component? e :direction))
                 [:set e [:direction] direction])])))))))

(defn update-key-shooters
  [{:keys [entities input-state time]}]
  (when (input/is-down? input-state :shoot)
    (util/flatten-1
      (-> entities
        (entities/those-with [:key-shooter :pos :direction])
        (entities/each
          (fn [{{:keys [delay-start shot-delay] :or {delay-start 0}} :key-shooter :as e}]
            (when (>= (- time delay-start) shot-delay)
              [[:create (game-entities/shot (-> e :pos :x) (-> e :pos :y) (:direction e))]
               [:set e [:key-shooter :delay-start] time]])))))))

(defn update-bomb-throwers
  [{:keys [entities input-state time]}]
  (when (input/is-down? input-state :throw)
    (util/flatten-1
      (-> entities
        (entities/those-with [:bomb-thrower :pos :direction :bomb-holder])
        (entities/each
          (fn [{{:keys [delay-start throw-delay] :or {delay-start 0}} :bomb-thrower :as e}]
            (when (and (>= (- time delay-start) throw-delay)
                       (pos? (:bomb-holder e))) 
              (let [direction (util/direction->angle
                                (if (input/is-down? input-state :move-up)
                                  :up
                                  (:direction e)))]
                [[:create (game-entities/bomb
                            (-> e :pos :x) (-> e :pos :y)
                            direction)]
                 [:update e [:bomb-holder] dec]
                 [:set e [:bomb-thrower :delay-start] time]]))))))))

(defn move-along-dimension
  [e dim distance dir solids]
  (loop [distance distance, e e]
    (if (pos? distance)
      (let [e- (update-in e [:pos dim] dir)]
        (if-let [collison (collision/check e- solids)]
          [e collison]
          (recur (dec distance) e-)))
      [e])))

(defn move-dynamic-bodies
  [{:keys [entities time-delta]}]
  (let [solids (entities/filter entities :solid?)]
    (util/flatten-1
      (-> entities
        (entities/those-with [:pos :vel :dynamic-body])
        (entities/each
          (fn [{{vx :x vy :y} :vel {:keys [stopped-by-solids?]} :dynamic-body :as e}]
            (if stopped-by-solids?
              (let [x-dir   (if (pos? vx) inc dec)
                    y-dir   (if (pos? vy) inc dec)
                    x-step  (-> vx (* time-delta) double Math/abs Math/floor)
                    y-step  (-> vy (* time-delta) double Math/abs Math/floor)]
                (when (or (pos? x-step) (pos? y-step))
                  (let [[e x-collision] (move-along-dimension e :x x-step x-dir solids)
                        [e y-collision] (move-along-dimension e :y y-step y-dir solids)]
                    (concat
                      [[:move e (:pos e)]]
                      (when x-collision
                        (concat
                          [[:set e [:vel :x] 0]]
                          (when (entity/has-component? e :collision-handler)
                            ((:collision-handler e) e x-collision))))
                      (when y-collision
                        (concat
                          [[:set e [:vel :y] 0]]
                          (when (entity/has-component? e :collision-handler)
                            ((:collision-handler e) e y-collision))))))))
              [[:move e {:x vx
                         :y vy
                         :relative? true}]])))))))

(def gravity {:y 800})

(defn apply-gravity
  [{:keys [entities]}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:vel :gravity])
      (entities/filter :gravity)
      (entities/each
        (fn [e]
          [[:accelerate e gravity]])))))

(defn update-fsm
  [{:keys [entities] :as game-state}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:state-machine])
      (entities/each
        #(fsm/update % game-state)))))

(defn move-camera
  [{:keys [entities]}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:camera-target :pos])
      (entities/each
        (fn [e]
          [[:center-camera-on e]])))))

(defn add-section-offsets
  [entities section-index]
  (let [offset (* section-index sections/pixel-width)]
    (map
      #(update-in % [:pos :x] + offset)
      entities)))

(defn wrap-as-create-statements
  [entities]
  (for [entity entities]
    [:create entity]))

(defn create-extension
  [section-index]
  (->
    (sections/any-template)
    sections/realize-template
    (add-section-offsets section-index)
    wrap-as-create-statements))

(defn extend-level
  [{:keys [entities camera last-section]}]
  (let [next-section  (-> camera
                        cam/right
                        (/ sections/pixel-width)
                        Math/floor)]
    (util/flatten-1
      (for [i (range (- next-section last-section))]
        (cons
          [:section-added]
          (create-extension (+ last-section i 1)))))))

(let [margin 500]
  (defn pace-the-beast
    [{:keys [entities camera]}]
    (util/flatten-1
      (-> entities
        (entities/of-type :beast)
        (entities/each
          (fn [beast]
            (when (> (- (cam/left camera) (entity/right beast))
                     margin)
              [[:move beast {:x (- (cam/left camera) margin)}]])))))))

(defn check-for-getting-eaten
  [{:keys [entities]}]
  (let [players (-> entities
                  (entities/of-type :player))]
    (util/flatten-1
      (-> entities
        (entities/of-type :beast)
        (entities/each
          (fn [beast]
            (when (collision/check beast players)
              [[:end]])))))))

(let [margin 100]
  (defn remove-passed-entities
    [{:keys [entities]}]
    (let [beast     (-> entities
                      (entities/of-type :beast)
                      entities/get-first)
          boundary    (- (entity/left beast) margin)]
      (-> entities
        (entities/those-with [:remove-when-passed?])
        (entities/filter :remove-when-passed?)
        (entities/each
          (fn [e]
            (when (< (entity/right e) boundary)
              [:destroy e])))))))

(let [margin 200]
  (defn remove-offscreen-entities
    [{:keys [entities camera]}]
    (-> entities
      (entities/those-with [:remove-when-offscreen?])
      (entities/filter :remove-when-offscreen?)
      (entities/each
        (fn [e]
          (when (or (< margin (- (cam/left camera) (entity/right e)))
                    (< margin (- (entity/left e) (cam/right camera)))
                    (< margin (- (cam/top camera) (entity/bottom e)))
                    (< margin (- (entity/top e) (cam/bottom camera))))
            [:destroy e]))))))

(defn collect-gems
  [{:keys [entities]}]
  (let [gem-collectors (-> entities
                         (entities/those-with [:gem-collector]))]
    (util/flatten-1
      (-> entities
        (entities/those-with [:gem])
        (entities/each
          (fn [gem]
            (when (collision/check gem gem-collectors)
              [[:destroy gem]
               [:collect-gem (:gem gem)]])))))))

(defn apply-friction
  [{:keys [entities time-delta]}]
  (let [walls (-> entities
                (entities/of-type :wall))]
    (-> entities
      (entities/those-with [:friction])
      (entities/each
        (fn [{:keys [friction] :as e}]
          (when (collision/below e walls)
            (let [xvel (-> e :vel :x)]
              (if (> (Math/abs xvel) (* friction time-delta))
                [:accelerate e {:x (* friction (Math/signum xvel) -1)}]
                [:accelerate e {:x 0
                                :relative? false}]))))))))

(defn explode-timers
  [{:keys [entities time]}]
  (util/flatten-1
  (-> entities
    (entities/those-with [:timed-explosion :pos])
    (entities/each
      (fn [{timer :timed-explosion :as e}]
        (when (and (:ticking timer)
                   (>= (- time (:start timer)) (:delay timer)))
          [[:destroy e]
           [:create (->
                      (game-entities/explosion 0 0 time)
                      (entity/center=
                        (entity/center-x e) (entity/center-y e)))]]))))))

(defn remove-timers
  [{:keys [entities time]}]
  (-> entities
    (entities/those-with [:timed-removal])
    (entities/each
      (fn [{timer :timed-removal :as e}]
        (when (>= (- time (:start timer)) (:delay timer))
          [:destroy e])))))

(defn explodify-explosions
  [{:keys [entities]}]
  (let [destructibles (-> entities
                        (entities/those-with [:destructible?])
                        (entities/filter :destructible?))]
    (util/flatten-1
      (-> entities
        (entities/those-with [:explosion])
        (entities/each
          (fn [e]
            (for [destroyed (collision/all e destructibles)]
              [:destroy destroyed])))))))
