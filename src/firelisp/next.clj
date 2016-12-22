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
  (if (and (seq? form) (= 'quote (first form)))
    form (t '~form)))

(defn with-defs
  "Return quoted body with function statements unquoted"
  [body]
  (let [defs (atom [])
        body (->> body
                  (paths/refer-special-forms)
                  (walk/postwalk (fn [x]
                                   (if (and (seq? x) (= 'firelisp.core/fn (first x)))
                                     (let [name (gensym)]
                                       (swap! defs conj (t (firelisp.core/def (quote ~name) ~x)))
                                       name)
                                     x))))]
    [@defs body]))

(defmacro expand
  "Expand Firelisp code"
  [body]
  (let [[defs body] (-> body
                        (paths/refer-special-forms)
                        (with-defs))]
    (t (do
         (println (quote (do
                           ~@defs
                           (firelisp.compile/expand ~(ensure-quote body)))))
         ~@defs
         (firelisp.compile/expand ~(ensure-quote body)))))
  )

