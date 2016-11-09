(ns firelisp.common
  (:require
    [firelisp.template :as t]
    [clojure.walk :as walk]))

(defn convert-quotes [form]
  (walk/postwalk (fn [x]
                   (if (and (seq? x)
                            (= (first x) 'quote)
                            (not (and (symbol? (second x))
                                      (= 2 (count x)))))
                     (cons 'firelisp.template/template (rest x))
                     x)) form))

(defmacro with-template-quotes [& forms]
  (t/template (do ~@(convert-quotes forms))))