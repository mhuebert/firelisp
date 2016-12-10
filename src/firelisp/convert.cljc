(ns firelisp.convert
  (:require [clojure.walk :as walk]))

(defn convert-quotes [form]
  (walk/postwalk (fn [x]
                   (if (and (seq? x)
                            (= (first x) 'quote)
                            (not (and (symbol? (second x))
                                      (= 2 (count x)))))
                     (cons 'firelisp.template/template (rest x))
                     x)) form))