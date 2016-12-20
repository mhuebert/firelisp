(ns firelisp.compile
  (:require [firelisp.template :refer [t] :include-macros true]
            [firelisp.specs :refer [get-arglists conf-meta]]
            [clojure.spec :as s :include-macros true]))

(defmacro defop [& body]
  (let [{:keys [name] :as conf} (s/conform :cljs.core/defn-args body)
        new-args (s/unform :cljs.core/fn-args conf)]
    (t (swap! firelisp.env/terminal-defs assoc (quote ~name)
              ~(assoc (conf-meta conf)
                 :value (t ~(cons 'cljs.core/fn new-args)))))))