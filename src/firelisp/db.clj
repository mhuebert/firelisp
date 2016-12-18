(ns firelisp.db
  (:refer-clojure :exclude [defn])
  (:require [firelisp.template :refer [template]]))

(defmacro at [& body]
  (template (firelisp.core/at ~@body)))

(defmacro rules [db & body]
  (template (firelisp.db/register-rules ~db (firelisp.core/at [] ~@body))))

(defmacro macro [db name & body]
  (let [body (cond-> body
                     (string? (first body)) rest)]
    (template (update ~db :functions assoc (quote ~name) (firelisp.core/macro ~@(cons name body))))))

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