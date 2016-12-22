(ns firelisp.next
  (:require [firelisp.template :refer [t] :include-macros true]
            [firelisp.env :refer [*defs* *context* resolve-sym]]
            [clojure.walk :as walk]
            [firelisp.paths :as paths]
            [firelisp.post-zip :as w]
            [clojure.zip :as z])
  (:require-macros [firelisp.next]))

(defn collect-path [loc]
  (loop [loc loc
         path '()]
    (if-not (z/up loc)
      (vec path)
      (recur (z/up loc)
             (let [form (z/node loc)]
               (cond-> path
                       (and (seq? form) (= 'path* (first form))) (into (reverse (second form)))))))))

(defn path-forms [body]
  (let [loc (z/seq-zip body)]
    (loop [loc (w/postorder-first loc)
           path-forms []]
      (if (z/end? loc)
        (t (do ~@path-forms))
        (let [form (z/node loc)
              collect? (and (seq? form) (= 'path* (first form)))]
          (recur (if collect? (-> (z/remove loc)
                                  (w/root)
                                  (w/postorder-first))
                              (w/postorder-next loc))
                 (cond-> path-forms
                         collect? (conj (t (firelisp.core/path** ~(collect-path loc) ~@(nnext form)))))))))))

(defn as-fn
  [f]
  (when (fn? f) f))

(defn expand-simple-1
  [loc]
  (walk/prewalk (fn [x]
                  (let [op (and (seq? x) (some-> (first x) (resolve-sym) (as-fn)))
                        replacement (when op (apply op (rest x)))]
                    (if (and op (not= replacement x))
                      replacement
                      x))) loc))

(defn expand-simple
  [body]
  (loop [current-expr (w/coll-zip body)
         count 0]
    (let [next-expr (expand-simple-1 current-expr)]
      (when (> count 100) (throw "Expand-fns probably in a loop, iterated 100 times"))
      (if (= next-expr current-expr)
        (z/root next-expr)
        (recur next-expr
               (inc count))))))

(defn resolve-expr
  [expr]
  (cond (symbol? expr)
        (or
          (get-in *defs* [(paths/munge-sym expr) :value])
          (get-in *context* [:bindings (paths/munge-sym expr)])
          expr)

        (and (seq? expr) (fn? (first expr)))
        (apply (first expr) (rest expr))

        :else expr))

(declare resolve-form*)

(defn let-context
  "Returns context as modified by a `let` form"
  [bindings]
  (loop [pairs (partition 2 bindings)
         context *context*]
    (if (empty? pairs)
      context
      (recur (rest pairs)
             (binding [*context* context]
               (update context :bindings assoc (ffirst pairs) (resolve-form* (second (first pairs)))))))))
(defn with-let
  "Bind symbols to values"
  [bindings body]
  (binding [*context* (let-context bindings)]
    (resolve-form* body)))

(defn resolve-form*
  [form]
  (cond
    (or (list? form)
        (seq? form)) (if (= 'let (paths/elide-core (first form)))
                       (apply with-let (rest form))
                       (let [f (some-> (first form) (resolve-sym) (as-fn))
                             new-res (and f (apply f (map resolve-form* (rest form))))]
                         (if (and f (not= form new-res))
                           (resolve-form* new-res)
                           (apply list (map resolve-form* form)))))
    (record? form) (reduce (fn [r x] (conj r (resolve-form* x))) form form)
    (coll? form) (into (empty form) (map resolve-form* form))
    (fn? form) (form)
    :else (resolve-expr form)))



(defn resolve-form [form]
  (do                                                       ;binding [*defs* (atom (assoc @*defs* 'let {:value with-let}))]
    (resolve-form* form)))

(defn expand-1
  [form]
  (walk/postwalk
    (fn [expr]
      (if (symbol? expr)
        (let [value (resolve-sym expr)]
          (if (fn? value) expr value))
        expr)) form))

(defn expand
  [expr]
  (loop [current-expr expr
         count 0]
    (let [next-expr (expand-1 current-expr)]
      (when (> count 100)
        (throw "Expand-fns probably in a loop, iterated 100 times"))
      (if (= next-expr current-expr)
        next-expr
        (recur next-expr
               (inc count))))))