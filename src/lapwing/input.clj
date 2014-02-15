(ns lapwing.input)

(defn create-state
  []
  (atom {:key-states  {}
         :aliases     {}}))

(defn def!
  [input & {:as aliases}]
  (doseq [[from to] aliases]
    (swap! input assoc-in [:aliases from] to)))

(defn set-state!
  [input key state]
  (swap! input assoc-in [:key-states key] state))

(defn is-down?
  [input key]
  (if-let [alias (get-in @input [:aliases key])]
    (is-down? input alias)
    (= :down (get-in @input [:key-states key]))))

(defn is-up?
  [input key]
  (not (is-down? input key)))
