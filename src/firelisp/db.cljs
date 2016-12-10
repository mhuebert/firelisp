(ns firelisp.db
  (:refer-clojure :exclude [set? set])
  (:require [static.targaryen]
            [firelisp.rules :refer [compile merge-rules] :include-macros true]
            [firelisp.standard-lib]
            [goog.object :as gobj])
  (:require-macros [firelisp.db]))

(defn log [x]
  #_(apply js/console.log args)
  x)

(defn ensure-rules [rules]
  (if (and rules (gobj/containsKey rules "tryRead"))
    rules
    (try
      (.ruleset js/targaryen (clj->js {:rules (or rules {})}))
      (catch js/Error e
        (.error js/console "Ensure-Rules Error!" e)
        (prn rules)))))

(defn database
  ([data] (database data (.now js/Date)))
  ([data now]
   (database (clj->js {"rules" {}}) data now))
  ([rules data now]
   (.database js/targaryen rules (clj->js data) now)))

(def blank
  {:rules     {}
   :rule-set  (ensure-rules {})
   :database  (database {} (.now js/Date))
   :functions @firelisp.compile/*rule-fns*
   :now       (.now js/Date)})

(defn compiled-rules
  ([db] (compiled-rules db (:rules db)))
  ([db rules]
   (binding [firelisp.compile/*rule-fns* (atom (:functions db))]
     (compile rules))))

(defn register-rules [db rules]
  (let [merged-rules (merge-rules (:rules db) rules)
        compiled-rules (compiled-rules db merged-rules)
        rule-set (ensure-rules compiled-rules)]
    (assoc db
      :database (.with (:database db) (clj->js {:rules {"rules" compiled-rules}}))
      :rules merged-rules
      :rule-set rule-set
      :compiled-rules compiled-rules)))

(defn try-write
  ([db path data] (try-write db path data (.now js/Date)))
  ([{:keys [database]} path data now]
   (.write database path (clj->js data) nil now)))

(defn set? [& args]
  (true? (-> (apply try-write args)
             (gobj/get "allowed"))))

(defn try-patch
  [{:keys [database]} path data & [now]]
  (.update database path (clj->js data) (or now (.now js/Date))))

(defn update? [& args]
  (true? (-> (apply try-patch args)
             (gobj/get "allowed"))))

(defn write
  [mode {:keys [database] :as db} path next-data now]
  (let [result (case mode
                 :set (.write database path (clj->js next-data) nil now)
                 :update (.update database path (clj->js next-data) now))]
    (if (.-allowed result)
      (assoc db :database (.-newDatabase result)
                :last-result result)
      (throw (js/Error "write not allowed")))))

(def set (partial write :set))

(defn set! [{:keys [database now] :as db} path next-data]
  (assoc db :database
            (.with database (clj->js {"data" (.$set (.-root database) path (clj->js next-data) nil now)}))))

(defn auth! [db auth]
  (assoc db :database
            (.with (:database db) (clj->js {"auth" auth}))))

(def update-data (partial write :update))

(defn try-read [{:keys [database rule-set auth]} path]
  (.read (.with database (clj->js {:rules rule-set
                                   :auth  auth}))
         path
         (new js/Date)))

(defn read [{:keys [database] :as db} path]
  (let [result (try-read db path)]
    (if (gobj/get result "allowed")
      (.val (.child (.snapshot database "/") path))
      (do (log (gobj/get result "info"))
          (throw (js/Error "read not allowed"))))))

(defn read? [& args]
  (gobj/get (apply try-read args) "allowed"))