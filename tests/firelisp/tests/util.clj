(ns firelisp.tests.util
  (:require [firelisp.template :refer [template]]))

(defmacro throws [& body]
  (let [docstring (when (string? (last body)) (last body))
        body (cond-> body
                     docstring drop-last)]
    (template (cljs.test/is (thrown? js/Error ~@body)
                            ~docstring))))

(defmacro isn't [& body]
  (let [docstring (when (string? (last body)) (last body))
        body (cond-> body
                     docstring drop-last)]
    (template (cljs.test/is (not ~@body)
                            ~docstring))))