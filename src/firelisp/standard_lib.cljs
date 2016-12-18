(ns firelisp.standard-lib
  (:refer-clojure :exclude [when nil? get get-in])
  (:require [cljs.core :as core]
            [firelisp.common :refer [append] :refer-macros [with-template-quotes]]
            [clojure.walk]
            [clojure.set :refer [subset?]]
            [firelisp.env :refer [*defs*]]
            [firelisp.paths :refer [parse-path throw-duplicate-path-variables]])
  (:require-macros [firelisp.core :as f]))

(defn flatten-nested [sym & [docstring]]
  {:name      sym
   :docstring docstring
   :fn        (with-template-quotes
                (fn [& form]
                  (loop [result '(~sym)
                         remaining form]
                    (if (empty? remaining)
                      result
                      (recur (if (and (seq? (first remaining))
                                      (= sym (ffirst remaining)))
                               (concat result (rest (first remaining)))
                               (append result (first remaining)))
                             (rest remaining))))))})

(defn flatten-child [& args]
  {:name      'flatten-child
   :docstring "Get child of data location in database"
   :fn        (with-template-quotes
                (if (and (seq? (first args))
                         (= 'child (ffirst args)))
                  '(child ~@(rest (first args)) ~@(rest args))
                  '(child ~@args)))})

(swap! *defs* merge

       {'and   (flatten-nested 'and "Boolean `and` (return true if all args are truthy)")
        'or    (flatten-nested 'or "Boolean `or` (return true if at least one arg is truthy)")
        'child flatten-child
        '+     (flatten-nested '+ "Javascript add (adds numbers, concatenates strings)")
        '*     (flatten-nested '* "Javascript multiply")})

(f/defn when [pred body]
        (if pred body true))

(f/defmacro cond [& args]
            (let [[[else expr] & pairs] (reverse (partition 2 args))]
              (loop [pairs pairs
                     expr (if (= :else else)
                            expr
                            '(if ~else ~expr false))]
                (if-let [[pred result] (first pairs)]
                  (recur (rest pairs)
                         '(if ~pred ~result ~expr))
                  expr))))

(defn root? [s]
  (= \/ (first (name (if (seq? s) (first s) s)))))

(f/defmacro ->
            "Threading macro"
            [start-point & operations]
            (loop [expr start-point
                   [next-op & remaining] operations]
              (if-not next-op
                expr
                (recur (if (seq? next-op)
                         '(~(first next-op) ~expr ~@(rest next-op))
                         '(~next-op ~expr))
                       remaining))))

(f/defmacro and->
            "Thread v through a series of forms, which are wrapped in `and`"
            [v & predicates]
            '(and ~@(for [p predicates
                          :let [[op & args] (if (seq? p) p '(~p))]]
                      '(~op ~v ~@args))))

(f/defn lower-case? [s]
        (= s (lower-case s)))

(f/defn upper-case? [s]
        (= s (upper-case s)))

(f/defn within [val min max]
        (and (>= val min) (<= val max)))

(f/defn between [val min max]
        (and (> val min) (< val max)))


(f/defmacro in-set?
            "Returns true if value is in coll, which must be a literal set (not a runtime/db value)."
            [coll value]
            {:pre [(set? coll)]}
            '(or ~@(for [option coll]
                     '(= ~value ~option))))

(f/defn string-length [s min max]
        (and (string? s)
             (between s min max)))

(f/defn new? []
        (and (= prev-data nil)
             (exists? next-data)))

(f/defn nil? [d]
        (= d nil))

(f/defn unchanged? []
        (= next-data prev-data))

(f/defmacro get-in
            ([target path]
              '(child ~target ~@path))
            ([target path default]
              '(if (exists? (child ~target ~@path))
                 (child ~target ~@path)
                 ~default)))

(f/defn get
        ([target attr]
          (child target attr))
        ([target attr default]
          (if (exists? (child target attr))
            (child target attr)
            default)))

(f/defmacro let [bindings body]
            (if (<= (count bindings) 2)
              (clojure.walk/postwalk-replace (apply hash-map bindings) body)
              (loop [result body
                     bindings (reverse (partition 2 bindings))]
                (if (empty? bindings)
                  result
                  (recur '(let [~@(first bindings)] ~result)
                         (rest bindings))))))