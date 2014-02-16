(ns lapwing.entity.fsm
  (:refer-clojure :exclude [def]))

(defmulti fsm-spec
  (fn [machine-name state-name]
    [machine-name state-name]))

(defn build-spec-map
  [specs]
  (loop [current-state nil, spec-map {}, specs specs]
    (if-let [spec (first specs)]
      (cond
        (symbol? spec)
        (recur (keyword spec)
               spec-map
               (rest specs))

        (list? spec)
        (let [[method-name params & body]  spec
              method-name                  (keyword method-name)]
        (recur current-state
               (assoc-in spec-map [current-state method-name]
                      `(fn ~params
                         ~@body))
               (rest specs))))
      spec-map)))

(defmacro def
  [machine-name & specs]
  (let [machine-name  (keyword machine-name)]
    `(do
       ~@(for [[state-name definition] (build-spec-map specs)]
           `(defmethod fsm-spec [~machine-name ~state-name]
              [_# _#]
              ~definition)))))

(defn change-state
  [{:keys [state-machine] :as entity} new-state]
  (let [begin (:begin (fsm-spec (:name state-machine) new-state))]
    (concat
      [[:set entity [:state-machine :state] new-state]]
      (begin entity))))

(defn update
  [{:keys [state-machine] :as entity} & args]
  (let [update (:update (fsm-spec (:name state-machine) (:state state-machine)))]
    (apply update entity args)))
