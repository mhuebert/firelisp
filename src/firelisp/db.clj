(ns firelisp.db
  (:refer-clojure :exclude [defn]))

(defmacro at [& body]
  `(~'firelisp.rules/at ~@body))

(defmacro rules [db & body]
  `(~'firelisp.db/register-rules ~db (~'firelisp.rules/at "/" ~@body)))

(defmacro defn [db name & body]
  (let [body (cond-> body
                     (string? (first body)) rest)]

    `(~'update ~db :functions assoc (quote ~name) (~'firelisp.rules/rulefn* ~@(cons name body)))))

(defmacro throws [& body]
  (let [docstring (when (string? (last body)) (last body))
        body (cond-> body
                     docstring drop-last)]
    `(~'cljs.test/is (~'thrown? ~'js/Error ~@body)
       ~docstring)))

(defmacro isn't [& body]
  (let [docstring (when (string? (last body)) (last body))
        body (cond-> body
                     docstring drop-last)]
    `(~'cljs.test/is (not ~@body)
       ~docstring)))