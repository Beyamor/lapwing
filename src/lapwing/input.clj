(ns lapwing.input)

(defn create-state
  []
  (atom {:current   {}
         :previous  {}
         :aliases   {}
         :pending  []}))

(defn def!
  [input & {:as aliases}]
  (doseq [[from to] aliases]
    (swap! input assoc-in [:aliases from] to)))

(defn set-state!
  [input key state]
  (swap! input update-in [:pending] conj [key state]))

(defn apply-pending
  [{:keys [pending] :as input}]
  (reduce
    (fn [input [key state]]
      (assoc-in input [:current key] state))
    input pending))

(defn update!
  [input]
  (swap! input
         #(-> %
            (assoc :previous (:current %))
            apply-pending
            (assoc :pending []))))

(defn is-down-in-period?
  [input period key]
  (if-let [alias (get-in input [:aliases key])]
    (if (coll? alias)
      (some #(is-down-in-period? input period %) alias)
      (is-down-in-period? input period alias))
    (= :down (get-in input [period key]))))

(defn is-down?
  [input key]
  (is-down-in-period? input :current key))

(defn is-up?
  [input key]
  (not (is-down? input key)))

(defn was-pressed?
  [input key]
  (and (is-down-in-period? input :current key)
       (not (is-down-in-period? input :previous key))))

(defn was-released?
  [input key]
  (and (not (is-down-in-period? input :current key))
       (is-down-in-period? input :previous key)))
