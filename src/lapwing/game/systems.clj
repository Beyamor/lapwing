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
  (let [template  (sections/any-template)]
    (->
      (sections/any-template)
      sections/template->entities
      (add-section-offsets section-index)
      wrap-as-create-statements)))

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
          (create-extension next-section))))))

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
