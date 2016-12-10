(ns firelisp.common
  (:require-macros [firelisp.template]))

(defn append [sq & xs]
  (if (vector? sq)
    (apply conj (cons sq xs))
    (concat sq xs)))