;; some documentation copied and modified from clojure.core, http://clojure.github.io/clojure/clojure.core-api.html#clojure.core

(ns firelisp.standard-lib
  (:refer-clojure :exclude [when nil? get get-in])
  (:require [cljs.core :as core]
            [firelisp.common :refer [append] :refer-macros [with-template-quotes]]
            [firelisp.template :refer [t] :include-macros true]
            [firelisp.specs :as specs]
            [clojure.walk]
            [clojure.set :refer [subset?]]
            [firelisp.next :as n]
            [firelisp.env :as env :refer [*defs* *rules* *context*]]
            [firelisp.paths :as paths]
            [cljs.spec :as s])
  (:require-macros [firelisp.core :as f]))

(defn make-fn [body]
  (let [conf (s/conform :firelisp.specs/fn-args body)
        bindings (:bindings firelisp.env/*context*)]
    (doto (core/fn [& args]
            (binding [firelisp.env/*context* (update firelisp.env/*context* :bindings merge bindings)]
              (let [{body :body arg-entry :args} (specs/get-entry conf (count args))]
                (n/resolve-form* (t (let ~(specs/merge-bindings arg-entry args)
                                      ~@body))))))
      (aset "fire$type" :fn))))

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
                             (rest remaining))))))
   :type      :macro})

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

(f/defmacro if-let
  ([bindings body]
   '(let ~bindings
      (if ~(first bindings) ~body)))
  ([bindings body else]
   '(let ~bindings
      (if ~(first bindings) ~body ~else))))

(f/defmacro when-let [bindings body]
  '(if-let ~bindings ~body))

(f/defmacro when [pred body]
  '(if ~pred ~body))

(f/defmacro cond [& clauses]
  (when-let [[test expr & more-clauses] (seq clauses)]
            (if (= :else test)
              expr
              (if (= 1 (count clauses))
                test
                (if (empty? more-clauses)
                  '(if ~test ~expr)
                  (if (= :let test)
                    '(let ~expr (cond ~@more-clauses))
                    (if (= :when test)
                      '(when ~expr (cond ~@more-clauses))
                      (if (= :when-let test)
                        '(when-let ~expr (cond ~@more-clauses))
                        '(if ~test ~expr (cond ~@more-clauses))))))))))

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

(f/defn true?
  "Returns true if x is equal to Boolean true"
  [x]
  (= true x))

(f/defn false?
  "Returns false if x is equal to Boolean false"
  [x]
  (= false x))

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
#_(f/defmacro get
    "Returns the child of a data object, not-found or nil if key not present."
    ([data-snapshot attr]
     '(child ~data-snapshot ~attr))
    ([data-snapshot attr not-found]
     '(if (exists? (child ~data-snapshot ~attr))
        (child ~data-snapshot ~attr)
        ~not-found)))

#_(f/defmacro *context* []
    firelisp.env/*context*)

#_(f/defmacro let
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



#_(f/defmacro let
    "Evaluates body in a lexical context in which the symbols in the binding-forms are bound to their respective init-exprs."
    [bindings body]
    (loop [[bindings body] [(partition 2 bindings) body]]
      (if (empty? bindings)
        body
        (recur (clojure.walk/postwalk-replace (apply hash-map (first bindings))
                                              [(rest bindings) body])))))



(f/defmacro fn
  "Returns a FireLisp function"
  [& body]
  (make-fn body))

(f/defmacro fn*
  [& body]
  (t (fn ~@body)))

(f/defmacro fn_* [& args]
  '(fn ~@args))

(f/defmacro let
  "Bind symbols to values"
  [bindings body]
  (binding [env/*context* (n/let-context bindings)]
    (n/resolve-form* body)))

(defn add [type rule]
  (swap! *rules* update-in (conj (:path *context*) type) (fnil (if (#{:index :children} type)
                                                                 (partial apply conj) conj) #{}) rule))


;; do the root expansion here?
#_(defmacro root
    ([data-snapshot ks]
     '(get-in data-snapshot ks))
    ([data-snapshot ks not-found]
     '(get-in data-snapshot ks not-found)))

(f/defmacro authorize
  [rule-map]
  (doseq [[rule-type rule] (seq rule-map)] (add rule-type rule)))

(defn validate-wrap [rule]
  (if (symbol? rule) (t (~rule next-data))
                     rule))

(f/defmacro validate
  ([rule-or-map]
   (if (map? rule-or-map) (validate nil rule-or-map)
                          (validate rule-or-map nil)))
  ([rule child-map]
   (core/when rule (add :validate (validate-wrap rule)))
   (doseq [[key rule] (seq child-map)]
     (when-not (:optional (meta rule))
       (add :children [key]))

     (binding [*context* (paths/context-with-path *context* [(name key)])]
       (cond (map? rule)
             (validate rule)

             (not (core/nil? rule)) (add :validate (validate-wrap rule))

             :else nil)))

    ;; removed
    #_(when (empty? (filter symbol? (keys child-rules)))
            (add 'other :validate false))
    #_(add :validate '(object? next-data))))


(f/defn object?
  [data-snapshot]
  (contains-keys? data-snapshot))