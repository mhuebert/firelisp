(ns firelisp.targaryen
  (:require [goog.object :as gobj]))

(defn ensure-rules [rules]
  (if (and rules (gobj/containsKey rules "tryRead"))
    rules
    (try
      (js/targaryen.utils.makeNewRuleSet (clj->js {:rules (or rules {})}))
      (catch js/Error e
        (.error js/console "Ensure-Rules Error!" e)
        (prn rules)))))

(defn database
  ([data] (database data (.now js/Date)))
  ([data now]
   (js/targaryen.utils.makeNewStore (clj->js data) now)))

(defn try-read
  ([data rules auth path] (try-read data rules auth path (.now js/Date)))
  ([data rules auth path now]
   (let [ruleset (ensure-rules rules)
         auth (clj->js auth)]
     (.tryRead ruleset path (cond-> data
                                    (map? data) (database now)) auth))))


(defn try-write
  ([write-mode prev-data rules auth path next-data] (try-write write-mode prev-data rules auth path next-data (.now js/Date)))
  ([write-mode prev-data rules auth path next-data now]
   (let [ruleset (ensure-rules rules)
         root (cond-> prev-data
                      (map? prev-data) (database now))
         auth (clj->js auth)]
     (case write-mode :set
                      (.tryWrite ruleset
                                 path
                                 root
                                 (clj->js next-data)
                                 auth
                                 nil nil nil now)
                      :update
                      (.tryPatch ruleset
                                 path
                                 root
                                 (clj->js next-data)
                                 auth
                                 nil nil nil now)))))

(defn read?
  ([data rules auth path] (read? data rules auth path (.now js/Date)))
  ([data rules auth path now]
   (:allowed (js->clj (try-read data rules auth path now) :keywordize-keys true))))

(defn write?
  ([root-data rules auth path next-data] (write? root-data rules auth path next-data (.now js/Date)))
  ([root-data rules auth path next-data now]
   (let [result (try-write :set root-data rules auth path next-data now)]
     ;(.log js/console result)
     (gobj/get result "allowed"))))

(defn update?
  ([root-data rules auth path next-data] (update? root-data rules auth path next-data (.now js/Date)))
  ([root-data rules auth path next-data now]
   (let [result (try-write :update root-data rules auth path next-data now)]
     ;(.log js/console result)
     (gobj/get result "allowed"))))