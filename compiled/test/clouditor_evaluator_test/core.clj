
(ns clouditor-evaluator-test.core
  (:require [clouditor-evaluator.core :refer [run-program]]
            [clojure.test :refer :all]))

(deftest
  run-test
  (let [code (slurp "examples/test-expression.ir")
        program (read-string code)]
    (run-program program)))
