(ns firelisp.common
  (:require
    [firelisp.convert :refer [convert-quotes]]))

(defmacro with-template-quotes [& forms]
  `(do ~@(convert-quotes forms)))