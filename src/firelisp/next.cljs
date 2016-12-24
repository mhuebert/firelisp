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

(defn as-sym
  [f]
  (when (symbol? f) f))

(defn resolve-expr
  [expr]
  (cond (symbol? expr)
        (get-in *context* [:bindings (paths/munge-sym expr)] expr)


        (and (seq? expr) (fn? (first expr)))
        (apply (first expr) (rest expr))

        (fn? expr)
        (expr)

        :else expr))

;; more controlled: if we can't resolve a symbol locally, unquote it?
;; standardize: macros get unevaluated args, functions don't
;; better handle on when functions capture their context. inline fns behave different than others.


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
                       (let [f (as-fn (or (some-> (first form) (as-sym) (resolve-sym))
                                          ;; unclear when the above ^^ captures
                                          ((first form))))
                             new-res (and f (apply f (map resolve-form* (rest form))))]
                         (if (and f (not= form new-res))
                           (resolve-form* new-res)
                           (apply list (map resolve-form* form)))))
    (record? form) (reduce (fn [r x] (conj r (resolve-form* x))) form form)
    (coll? form) (into (empty form) (map resolve-form* form))
    :else (resolve-expr form)))

(defn resolve-form [form]
  (resolve-form* form))