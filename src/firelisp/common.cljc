(ns firelisp.common
  (:require
    [clojure.walk :as walk]))

(defn append [sq & xs]
  (if (vector? sq)
    (apply conj (cons sq xs))
    (concat sq xs)))

(defn convert-quotes [form]
  (walk/postwalk (fn [x]
                   (if (and (seq? x)
                            (= (first x) 'quote)
                            (not (and (symbol? (second x))
                                      (= 2 (count x)))))
                     (cons 'firelisp.backtick/template (rest x))
                     x)) form))