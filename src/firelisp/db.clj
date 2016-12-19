(ns firelisp.db
  (:require [firelisp.template :refer [template]]))

(defmacro rules [db & body]
  (template (firelisp.db/register-rules ~db (firelisp.core/at [] ~@body))))

(defmacro macro [db & body]
  (template
    (let [{name# :name :as macro#} (firelisp.core/macro ~@body)]
      (update ~db :functions assoc name# macro#))))

