(ns firelisp.db
  (:refer-clojure :exclude [set?])
  (:require [static.targaryen]
            [firelisp.rules :refer [compile merge-rules] :include-macros true]
            [firelisp.standard-lib]
            [goog.object :as gobj]
            [firelisp.targaryen :as targar])
  (:require-macros [firelisp.db]))

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
     (compile rules))))

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
  (-> (apply try-write args)
      (gobj/get "allowed")
      true?))

(defn try-patch
  ([db path data] (try-write db path data nil))
  ([{:keys [database rule-set auth]} path data now]
   (.tryPatch rule-set path database (clj->js data) auth nil nil nil now)))

(defn update? [& args]
  (-> (apply try-patch args)
      (gobj/get "allowed")
      true?))

(defn write [mode {:keys [auth] :as db} path next-data]
  (let [f (case mode :set try-write :update try-patch)
        result (f db path (clj->js next-data))]
    (if (gobj/get result "allowed")
      (assoc db :database (gobj/get result "newDatabase")
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

(defn read [{:keys [database] :as db} path]
  (let [result (try-read db path)]
    (if (gobj/get result "allowed")
      (.val (.child (.snapshot database "/") path))
      (do (log (gobj/get result "info"))
          (throw (js/Error "read not allowed"))))))

(defn read? [& args]
  (gobj/get (apply try-read args) "allowed"))