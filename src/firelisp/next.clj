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

(defn unquote-fns
  "Return quoted body with function statements unquoted"
  [body]
  (let [i (atom {})
        body (->> body
                  (walk/postwalk (fn [x]
                                   (if (and (seq? x)
                                            (#{'fn
                                               'fn*
                                               'defn
                                               'macro
                                               'defmacro} (some-> (first x) paths/elide-core)))
                                     (let [name (gensym)]
                                       (swap! i assoc (t (quote ~name)) x)
                                       name)
                                     x))))]
    (t (->> ~(ensure-quote body)
            (clojure.walk/postwalk-replace ~(deref i))))))

(defmacro expand
  "Expand Firelisp code"
  [body]
  (t (-> ~(-> body
              (paths/refer-special-forms)
              (unquote-fns))
         #_(firelisp.next/expand-simple)
         (firelisp.next/resolve-form))))

