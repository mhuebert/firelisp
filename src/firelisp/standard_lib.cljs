;; some documentation copied and modified from clojure.core, http://clojure.github.io/clojure/clojure.core-api.html#clojure.core

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
   :value     (with-template-quotes
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
   :value     (with-template-quotes
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
  "Threading macro. Threads the expr through the forms. Inserts x as the\nsecond item in the first form, making a list of it if it is not a\nlist already. If there are more forms, inserts the first form as the\nsecond item in second form, etc."
  [x & forms]
  (loop [expr x
         [next-op & remaining] forms]
    (if-not next-op
      expr
      (recur (if (seq? next-op)
               '(~(first next-op) ~expr ~@(rest next-op))
               '(~next-op ~expr))
             remaining))))

(f/defmacro and->
  "Thread x through a series of forms, wrapped in `and`"
  [x & predicates]
  '(and ~@(for [p predicates
                :let [[op & args] (if (seq? p) p '(~p))]]
            '(~op ~x ~@args))))

(f/defn lower-case?
  "Returns true if string is lowercase."
  [s]
  (= s (lower-case s)))

(f/defn upper-case?
  "Returns true if string is uppercase."
  [s]
  (= s (upper-case s)))

(f/defn within
  "Returns true if val is within min and max, inclusive."
  [val min max]
  (and (>= val min) (<= val max)))

(f/defn between
  "Returns true if val is between min and max, exclusive."
  [val min max]
  (and (> val min) (< val max)))

(f/defmacro set-contains?
  "Returns true if value is in set, which must be a literal set (not a runtime value)."
  [set value]
  {:pre [(set? set)]}
  '(or ~@(for [option set]
           '(= ~value ~option))))

(f/defn new?
  "Returns true if data-snapshot was empty prior to a :write/:validate rule."
  []
  (and (= prev-data nil)
       (exists? next-data)))

(f/defn nil?
  "Returns true if value is nil."
  [d]
  (= d nil))

(f/defn unchanged?
  "Returns true if data-snapshot will not be modified by a :write/:validate rule."
  []
  (= next-data prev-data))

(f/defmacro get-in
  "Returns the value in a nested data object, not-found or nil if key not present."
  ([data-snapshot ks]
   '(child ~data-snapshot ~@ks))
  ([data-snapshot ks not-found]
   '(if (exists? (child ~data-snapshot ~@ks))
      (child ~data-snapshot ~@ks)
      ~not-found)))

(f/defn get
  "Returns the child of a data object, not-found or nil if key not present."
  ([data-snapshot attr]
   (child data-snapshot attr))
  ([data-snapshot attr not-found]
   (if (exists? (child data-snapshot attr))
     (child data-snapshot attr)
     not-found)))

(f/defmacro let
  "Evaluates body in a lexical context in which the symbols in the binding-forms are bound to their respective init-exprs."
  [bindings body]
  (if (<= (count bindings) 2)
    (clojure.walk/postwalk-replace (apply hash-map bindings) body)
    (loop [result body
           bindings (reverse (partition 2 bindings))]
      (if (empty? bindings)
        result
        (recur '(let [~@(first bindings)] ~result)
               (rest bindings))))))

(f/defn object?
  [data-snapshot]
  (contains-keys? data-snapshot))