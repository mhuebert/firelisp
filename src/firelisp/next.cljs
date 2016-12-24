(ns firelisp.next
  (:require [firelisp.template :refer [t] :include-macros true]
            [clojure.walk :as walk]
            [firelisp.paths :as paths]
            [firelisp.post-zip :as w]
            [clojure.zip :as z]
            [firelisp.env :as env :refer [*defs* *context* resolve-sym]]
            [firelisp.specs :as specs]
            [clojure.string :as string]
            [cljs.spec :as s])
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
        (get-in *context* [:bindings (paths/munge-sym expr) :value]
                (if (env/resolve-var expr)
                  expr
                  (throw (js/Error (str "Symbol not found: " expr)))))


        (and (seq? expr) (fn? (first expr)))
        (apply (first expr) (rest expr))

        (fn? expr)
        expr

        :else expr))

;; more controlled: if we can't resolve a symbol locally, unquote it?
;; standardize: macros get unevaluated args, functions don't
;; better handle on when functions capture their context. inline fns behave different than others.


(declare resolve-form*)

(defn let-context
  "Returns context as modified by a `let` form"
  [bindings]
  (loop [pairs (partition 2 (mapcat (fn [[name value]] (-> (s/conform :firelisp.specs/binding-form name)
                                                           (specs/destructure-arg)
                                                           (specs/binding-assignments value))) (partition 2 bindings)))
         context *context*]
    (if (empty? pairs)
      context
      (recur (rest pairs)
             (binding [*context* context]
               (let [name (ffirst pairs)
                     value (resolve-form* (second (first pairs)))]
                 (update context :bindings assoc (ffirst pairs) {:name     name
                                                                 :value    value
                                                                 :binding? true
                                                                 :type     (some-> value (aget "fire$type"))})))))))

(defn resolve-form*
  [form]
  (cond
    (or (list? form)
        (seq? form)) (let [op (resolve-form* (first form))
                           {:keys [type value]} (if (fn? op)
                                                  {:value op}
                                                  (some-> (first form) (as-sym) (env/resolve-var)))
                           f (when (and (not= type :terminal-op) (fn? value)) value)
                           new-res (and f (case (or (aget f "fire$type") type)
                                            :fn (apply f (map resolve-form* (rest form)))
                                            :macro (apply f (rest form))
                                            (throw (js/Error "Not sure what kind of operator this is.." form f))))]
                       (if (and f (not= form new-res))
                         (resolve-form* new-res)
                         (apply list (map resolve-form* form))))
    (record? form) (reduce (fn [r x] (conj r (resolve-form* x))) form form)
    (coll? form) (into (empty form) (map resolve-form* form))
    :else (resolve-expr form)))

(defn resolve-form [form]
  (resolve-form* form))