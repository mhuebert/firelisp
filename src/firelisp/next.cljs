(ns firelisp.next
  (:require [firelisp.env :refer [*defs* *context*]]
            [clojure.walk :as walk]
            [firelisp.paths :as paths])
  (:require-macros [firelisp.next]))

(defn expand-1
  ([form] (expand-1 @*defs* form))
  ([defs form]
   (walk/postwalk
     (fn [expr]
       (cond (symbol? expr)
             (or
               (get-in defs [(paths/munge-sym expr) :value])
               (get-in *context* [:bindings (paths/munge-sym expr)])
               expr)

             (and (seq? expr) (fn? (first expr)))
             (apply (first expr) (rest expr))

             :else expr)) form)))

(defn expand
  [expr]
  (loop [current-expr expr
         count 0]
    (let [next-expr (expand-1 @*defs* current-expr)]
      (when (> count 100)
        (throw "Expand-fns probably in a loop, iterated 100 times"))
      (if (= next-expr current-expr)
        next-expr
        (recur next-expr
               (inc count))))))