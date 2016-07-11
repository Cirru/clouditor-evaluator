
(ns clouditor.core)

(defn wrap [f w] (fn [& args] (apply f (map w args))))

(defn evaluate-expression [scope expression])

(defn add-definition [top-scope binding-name expression])

(defn load-definitions [top-scope definitions]
  (if (empty? definitions)
    top-scope
    (let [cursor (first definitions)
          binding-name (first cursor)
          expression (last cursor)
          next-scope (add-definition
                       top-scope
                       binding-name
                       expression)]
      (recur top-scope (subvec definitions)))))