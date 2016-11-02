(ns firelisp.db
  #?(:cljs
     (:refer-clojure :exclude [set?])
     :clj
     (:refer-clojure :exclude [defn]))
  #?(:cljs
     (:require [static.targaryen]
       [firelisp.ruleset :refer [compile-map merge-rules] :include-macros true]
       [firelisp.standard-lib]
       [firelisp.targaryen :as targar])
     :clj
     (:require
       [firelisp.common :refer [convert-quotes]]
       [firelisp.ruleset :refer [with-template-quotes]]
       [firelisp.backtick :refer [template]]))
  #?(:cljs
     (:require-macros [firelisp.db])))


#?(:clj
   (do
     (defmacro at [& body]
       `(firelisp.ruleset/at ~@body))

     (defmacro rules [db & body]
       (with-template-quotes
         '(firelisp.db/register-rules ~db (firelisp.ruleset/at "/" ~@body))))

     (defmacro defn [db name & body]
       (let [body (cond-> body
                          (string? (first body)) rest)]
         (with-template-quotes
           '(update ~db :functions assoc (quote ~name) (firelisp.ruleset/rulefn* ~@(cons name body))))))

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
                                 ~docstring))))))

#?(:cljs
   (do

     (defn log [x]
       #_(apply js/console.log args)
       x)

     (def blank
       {:rules          {}
        :compiled-rules {}
        :rule-set       (targar/ensure-rules {})
        :database       (targar/database {} (.now js/Date))
        :functions      @firelisp.compile/*rule-fns*
        :now            (.now js/Date)})

     (defn compiled-rules
       ([db] (compiled-rules db (:rules db)))
       ([db rules]
        (binding [firelisp.compile/*rule-fns* (atom (:functions db))]
          (compile-map rules))))

     (defn register-rules [db rules]
       (let [merged-rules (merge-rules (:rules db) rules)
             compiled-rules (compiled-rules db merged-rules)
             rule-set (targar/ensure-rules compiled-rules)]
         (assoc db
           :rules merged-rules
           :compiled-rules compiled-rules
           :rule-set rule-set)))

     (defn try-write
       ([db path data] (try-write db path data nil))
       ([{:keys [database rule-set auth]} path data now]
        (.tryWrite rule-set path database (clj->js data) auth nil nil nil now)))

     (defn set? [& args]
       (true? (.-allowed (apply try-write args))))

     (defn try-patch
       ([db path data] (try-write db path data nil))
       ([{:keys [database rule-set auth]} path data now]
        (.tryPatch rule-set path database (clj->js data) auth nil nil nil now)))

     (defn update? [& args]
       (.-allowed (apply try-patch args)))

     (defn write [mode {:keys [auth] :as db} path next-data]
       (let [f (case mode :set try-write :update try-patch)
             result (f db path (clj->js next-data))]
         (if (.-allowed result)
           (assoc db :database (.-newDatabase result)
                     :last-result result)
           (do #_(log (.-info result))
             #_(.log js/console result)
             (throw (js/Error "write not allowed"))))))

     (def set-data (partial write :set))

     (defn set! [{:keys [database now] :as db} path next-data]
       (assoc db :database (.set database path (clj->js next-data) now)))

     (defn auth! [db auth]
       (assoc db :auth (clj->js auth)))

     (def update-data (partial write :update))

     (defn try-read [{:keys [database rule-set auth]} path]
       (.tryRead rule-set path database auth (new js/Date)))

     (defn read? [& args]
       (.-allowed (apply try-read args)))

     (defn read [{:keys [database] :as db} path]
       (let [result (try-read db path)]
         (if (.-allowed result)
           (.val (.child (.snapshot database "/") path))
           (do (log (.-info result))
               (throw (js/Error "read not allowed"))))))))