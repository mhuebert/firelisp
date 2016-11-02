(ns firelisp.standard-lib
  (:require [firelisp.common :refer [append]]
            [clojure.set :refer [subset?]]
            [firelisp.compile :refer [*rule-fns*]]
            [firelisp.paths :refer [parse-path throw-duplicate-path-variables]])
  (:require-macros [firelisp.ruleset :refer [rulefn with-template-quotes]]))

(defn simplify [sym]
  (with-template-quotes
    (fn [& form]
      (loop [result '(~sym)
             remaining form]
        (if (empty? remaining)
          result
          (recur (if (and (seq? (first remaining))
                          (= sym (ffirst remaining)))
                   (concat result (rest (first remaining)))
                   (append result (first remaining)))
                 (rest remaining)))))))

(defn simplify-child [& args]
  (with-template-quotes
    (if (and (seq? (first args))
             (= 'child (ffirst args)))
      '(child ~@(rest (first args)) ~@(rest args))
      '(child ~@args))))

(swap! *rule-fns* merge {'and   (simplify 'and)
                         'or    (simplify 'or)
                         'child simplify-child
                         '+     (simplify '+)
                         '*     (simplify '*)})

(defn condition-preds [target]
  (with-template-quotes
    {:create '(and (nil? (prior ~target)) (exists? ~target))
     :update '(and (not= nil (prior ~target))
                   (not= nil ~target)
                   (not= (prior ~target) ~target))
     :delete '(and (exists? (prior ~target)) (not (exists? ~target)))}))

(rulefn when [pred body]
        '(if ~pred ~body true))

(rulefn on [& args]
        (let [target? (not (or (keyword? (first args))
                               (vector? (first args))))
              target (if target? (first args) 'data)
              conditions (if target? (second args) (first args))
              body (last args)
              conditions (-> (condition-preds target)
                             (select-keys (if (vector? conditions) conditions [conditions]))
                             vals)]
          ;(prn "on" args)
          #_(subset? (set conditions) #{:create :update :delete})
          '(when (or ~@conditions) ~body)))

(rulefn cond [& args]
        (loop [pairs (drop-last (partition 2 args))
               expr (last args)]
          (if-let [[pred result] (last pairs)]
            (recur (drop-last pairs)
                   '(if ~pred ~result ~expr))
            expr)))

(defn root? [s]
  (= \/ (first (name (if (seq? s) (first s) s)))))

(rulefn -> [start-point & operations]
        (loop [expr start-point
               [next-op & remaining] operations]
          (if-not next-op
            expr
            (recur (if (seq? next-op)
                     '(~(first next-op) ~expr ~@(rest next-op))
                     '(~next-op ~expr))
                   remaining))))

(rulefn every-> [v & predicates]
        (loop [expr '(and)
               [next-op & remaining] predicates]
          (if-not next-op
            expr
            (recur (append expr
                           (if (seq? next-op)
                             '(~(first next-op) ~v ~@(rest next-op))
                             '(~next-op ~v)))
                   remaining))))

(rulefn lower-case? [s]
        '(= ~s (lower-case ~s)))

(rulefn upper-case? [s]
        '(= ~s (lower-case ~s)))

(rulefn within [val min max]
        '(and (>= ~val ~min) (<= ~val ~max)))

(rulefn between [val min max]
        '(and (> ~val ~min) (< ~val ~max)))

(rulefn in?
        "Returns true if value is in coll, which must be a set."
        [coll value]
        {:pre [(set? coll)]}
        '(or ~@(for [option coll]
                 '(= ~value ~option))))

(rulefn string-length [s min max]
        '(and (string? ~s)
              (between ~s ~min ~max)))

(rulefn new? [d]
        '(and (= (prior ~d) nil)
              (exists? ~d)))

(rulefn nil? [d]
        '(= ~d nil))

(rulefn unchanged? [d]
        '(= ~d (prior ~d)))

(rulefn get-in
        ([target path]
          '(child ~target ~@path))
        ([target path default]
          '(if (exists? (child ~target ~@path))
             (child ~target ~@path)
             ~default)))

(rulefn get
        ([target attr]
          '(child ~target ~attr))
        ([target attr default]
          '(if (exists? (child ~target ~attr))
             (child ~target ~attr)
             ~default)))

(rulefn let [bindings & body]
        (clojure.walk/postwalk-replace (apply hash-map bindings) (last body)))
