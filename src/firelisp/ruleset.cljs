(ns firelisp.ruleset
  (:require
    [firelisp.compile :refer [compile *path*]]
    [firelisp.common :refer [append]]
    [firelisp.paths :refer [parse-path throw-duplicate-path-variables]]
    [firelisp.targaryen :refer [ensure-rules try-read try-write]])
  (:require-macros
    [firelisp.ruleset :refer [add at with-template-quotes]]))

(def ^:dynamic *rules* nil)


(defn update-in-map
  "Update-in where the empty vector [] does not behave as [nil]"
  [m ks f & args]
  (if (seq ks) (apply update-in m ks f args) (apply f m args)))

(defn pfx [prefix m]
  (reduce-kv (fn [m k v]
               (cond-> m
                       (cljs.core/string? v) (assoc k (str prefix "." v))))
             m m))

(def auth (clj->js (pfx "auth" {:provider "provider"
                                :token    (pfx "auth.token" {:email                     "email"
                                                             :email_verified            "email_verified"
                                                             :name                      "name"
                                                             :sub                       "sub"
                                                             :firebase.identities       "firebase.identities"
                                                             :firebase.sign_in_provider "firebase.sign_in_provider"})
                                :uid      "uid"})))



(declare compile-map)

(defn filter-by-value [pred m]
  (reduce-kv (fn [m k v] (cond-> m
                                 (pred v) (assoc k v)
                                 (map? v) (assoc k (filter-by-value pred v)))) {} m))

(letfn [(merge-in* [a b]
          (cond (map? a)
                (merge-with merge-in* a b)
                (and (set? a) (set? b))
                (into a b)
                :else b))]
  (defn merge-rules
    "Merge multiple nested maps."
    [& args]
    (reduce merge-in* nil args)))

(defn condition-preds [target]
  (with-template-quotes
    {:create '(and (nil? (prior ~target)) (exists? ~target))
     :update '(and (not= nil (prior ~target))
                   (not= nil ~target)
                   (not= (prior ~target) ~target))
     :delete '(and (exists? (prior ~target)) (not (exists? ~target)))}))

(def cud-preds (condition-preds 'data))

(defn log [x] (prn x) x)
(defn compile-map
  ([rules] (compile-map rules [] 0))
  ([{:keys [create read update delete index write validate children] :as rules} path depth]
   (with-template-quotes
     (let [validate (cond-> validate
                            (seq children) (disj '(object? data)))]
       (merge
         (binding [*path* path]
           (cond-> {}
                   (seq read) (assoc ".read" (compile {:mode :read}
                                                      '(and ~@read)))

                   (first (keep seq [write create update delete]))
                   (assoc ".write" (compile {:mode :write}
                                            (if (seq write)
                                              (cond-> '(and ~@write)
                                                      (seq create) (append '(when ~(:create cud-preds) (and ~@create)))
                                                      (seq update) (append '(when ~(:update cud-preds) (and ~@update)))
                                                      (seq delete) (append '(when ~(:delete cud-preds) (and ~@delete))))
                                              (cond->> 'false
                                                       (seq create) (append '(if ~(:create cud-preds) (and ~@create)))
                                                       (seq update) (append '(if ~(:update cud-preds) (and ~@update)))
                                                       (seq delete) (append '(if ~(:delete cud-preds) (and ~@delete)))))))
                   (seq index) (assoc ".indexOn" (vec index))
                   (or (seq validate)
                       (seq children)) (assoc ".validate"
                                              (compile {:mode :validate}
                                                       (cond-> '(and)
                                                               (seq children)
                                                               (append '(has-children? data [~@children]))
                                                               (seq validate)
                                                               (append '(do ~@validate)))))))
         (reduce-kv (fn [m k v]
                      (assoc m (munge k)
                               (compile-map v (conj path k) (inc depth))))
                    {}
                    (dissoc rules :create :read :update :delete :index :write :validate :children)))))))


(defn wrap-rule [rule]
  (with-template-quotes
    (if (symbol? rule) '(~rule data)
                       rule)))

(defn validate
  ([one-arg]
   (if (map? one-arg) (validate nil one-arg)
                      (validate one-arg nil)))
  ([root-rule child-rules]
   (with-template-quotes
     (when root-rule
       (add :validate (wrap-rule root-rule)))

     (when child-rules
       (doseq [[child-name rule] (seq child-rules)]
         (cond (map? rule)
               (at (munge (name child-name))
                   {:validate rule})

               (not (nil? rule))
               (add child-name :validate (wrap-rule rule))

               :else nil)

         (when (and (not (:optional (meta rule)))
                    (not (= \$ (first (name child-name)))))
           (add :children [child-name])))

       (when (empty? (filter #(= \$ (first (name %))) (keys child-rules)))
         (add "$other" :validate false))

       (add :validate '(object? data))))))