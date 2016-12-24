(ns firelisp.next
  (:require [firelisp.template :refer [t] :include-macros true]
            [firelisp.paths :as paths]
            [clojure.walk :as walk]))

(defmacro authorize
  "Returns a map of `authorize` rules. Binds arglist to [prev-data, next-data]."
  [arglist rule-map]
  (t
    (let [bindings# '~(apply hash-map (interleave arglist ['prev-data 'next-data]))]
      (reduce-kv (fn [m# k# v#]
                   (update m# k# (fnil conj []) {:context {:bindings (merge (:bindings firelisp.env/*context*) bindings#)}
                                                 :rule    v#})) {} ~(paths/refer-special-forms rule-map)))

    ))

(defmacro validate [bindings rule-map]
  `(authorize ~bindings {:validate ~rule-map}))

(defmacro path [path-segments & body]
  `(binding [~'firelisp.env/*rules* (or ~'firelisp.env/*rules* (atom {}))
             ~'firelisp.env/*context* (paths/context-with-path ~'firelisp.env/*context* (quote ~path-segments))]
     (merge ~@(paths/refer-special-forms body))))

(defn ensure-quote [form]
  (if (and (seq? form) (#{'quote 'firelisp.template/t} (first form)))
    form (t (firelisp.template/t ~form))))

;; takes forever to compile
#_(defn unquote-locals [&env form]
  (t (let ~(vec (apply hash-map (for [k (get &env :locals)]
                                  [k (list 'clojure.core/unquote k)]))) ~form)))

;; idea: unquote anything that is not in scope

(defmacro expand
  "Expand Firelisp code"
  [body]
  (t (-> ~(->> body
               (paths/refer-special-forms)
               ensure-quote)
         (firelisp.next/resolve-form))))

(defmacro let-context-macro [body]
  (t (-> ~(->> body
               (paths/refer-special-forms)
               ensure-quote)
         (firelisp.next/let-context))))