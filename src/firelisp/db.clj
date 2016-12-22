(ns firelisp.db
  (:require [firelisp.template :refer [t]]))

(defmacro rules [db & body]
  (t (firelisp.db/register-rules ~db (firelisp.core/path [] ~@body))))

(defmacro macro [db & body]
  (t
    (let [{name# :name :as macro#} (firelisp.core/macro* ~body)]
      (update ~db :functions assoc name# macro#))))