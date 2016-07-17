
(ns clouditor-evaluator.util.error)

(defn raise [x] (throw (Error. x)))
