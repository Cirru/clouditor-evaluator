
(ns clouditor-evaluator.core
  (:require [clouditor-evaluator.util.error :refer [raise]]
            [clouditor-evaluator.util.reader :refer [parse-string]]))

(defn read-token [scope token]
  (cond
    (= token "true") true
    (= token "false") false
    (re-find (re-pattern "^\\d+$") token) (parse-string token)
    :else (if (contains? scope token)
            (get scope token)
            (raise (str (pr-str token) " not recognized!")))))

(declare read-item)

(defn read-expression [scope expression]
  (if (empty? expression)
    (raise "empty expression!")
    (let [header (first expression)]
      (case
        header
        "return"
        (let [x (read-item scope (get expression 1))]
          {:type "Monad", :data x})
        "\\"
        (if (= (count expression) 2)
          (fn [] (read-item scope (get expression 1)))
          (fn [& args]
            (let [arg-names (get expression 1)]
              (if (not= (count arg-names) (count args))
                (raise "Arguments not matching")
                (let [new-scope (reduce
                                  (fn 
                                    [acc cursor]
                                    (let 
                                      [[arg-name arg-value] cursor]
                                      (assoc acc arg-name arg-value)))
                                  (map vector arg-names args))]
                  (read-item new-scope (get expression 2)))))))
        "let-bind"
        (if (not= (count expression) 3)
          (raise "let-bind expects 2 arguments")
          (let [value (read-item scope (get expression 1))
                caller (read-expression scope (get expression 2))]
            (if (not (fn? caller))
              (raise "let-bind expects a function as a caller")
              (caller value))))
        "io-bind"
        (if (not= (count expression) 3)
          (raise "io-bind expects 2 arguments")
          (let [value (read-item scope (get expression 1))
                caller (read-expression scope (get expression 2))]
            (if (not (fn? caller))
              (raise "io-bind expects a function as a caller")
              (if (= (:type value) "Monad")
                (caller (:data value))
                (raise "io-bind excepts a monadic value")))))
        "println"
        (do
          (apply
            println
            (map (fn [x] (read-item scope x)) (rest expression)))
          {:type "Monad", :data nil})
        "+"
        (apply + (map (fn [x] (read-item scope x)) (rest expression)))
        "*"
        (apply * (map (fn [x] (read-item scope x)) (rest expression)))
        "read-file"
        {:type "Monad", :data "faked file data..."}
        (let [maybe-func (read-item scope header)]
          (if (fn? maybe-func)
            (apply maybe-func (map read-item (rest expression)))
            (raise (str (pr-str header) " is not function!"))))))))

(defn read-item [scope x]
  (if (string? x) (read-token scope x) (read-expression scope x)))

(defn add-definition [top-scope binding-name expression])

(defn load-definitions [scope definitions]
  (if (empty? definitions)
    scope
    (let [cursor (first definitions)
          binding-name (first cursor)
          expression (last cursor)
          next-scope (assoc
                       scope
                       binding-name
                       (read-expression scope expression))]
      (recur scope (subvec definitions 1)))))

(defn run-program [program]
  (let [scope (load-definitions {} program) main (:main scope)] (main)))
