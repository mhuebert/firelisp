(ns firelisp.compile
  (:require [firelisp.template :refer [template] :include-macros true]
            [firelisp.specs :refer [get-arglists]]
            [clojure.spec :as s :include-macros true]))

(defmacro defop [& body]
  (let [{:keys [name docstring] :as conf} (s/conform :cljs.core/defn-args body)
        new-args (s/unform :cljs.core/fn-args conf)]
    (template
      (do
        (println (quote {:name      (quote ~name)
                         :docstring ~docstring
                         :arglists  ~(get-arglists conf)
                         :fn        ~(cons 'cljs.core/fn new-args)}))

        (swap! firelisp.compile/operators* assoc (quote ~name)
               {:name      (quote ~name)
                :docstring ~docstring
                :fn        ~(cons 'cljs.core/fn new-args)})))))