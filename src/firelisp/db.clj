(ns firelisp.db
  (:refer-clojure :exclude [defn])
  (:require [firelisp.template :refer [template]]))

(defmacro at [& body]
  (template (firelisp.core/at ~@body)))

(defmacro rules [db & body]
  (template (firelisp.db/register-rules ~db (firelisp.core/at [] ~@body))))

(defmacro macro [db & body]
  (template
    (let [{name# :name :as macro#} (firelisp.core/macro ~@body)]
      (update ~db :functions assoc name# macro#))))

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