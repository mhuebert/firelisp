(ns firelisp.core
  (:require
    [static.targaryen]
    [firelisp.env :refer [*rules* *context*]]
    [firelisp.repl]
    [firelisp.next :include-macros true]
    [firelisp.standard-lib]
    [firelisp.compile :refer [compile-expr]]
    [firelisp.common :refer [append] :refer-macros [with-template-quotes]]
    [firelisp.paths :refer [parse-path throw-duplicate-path-variables context-with-path]])
  (:require-macros
    [firelisp.core :refer [path]]))

(defn add
  [path & rules]
  (try (let [[path rules] (if (odd? (count rules))
                            ["" (cons path rules)]
                            [path rules])
             parsed-path (firelisp.paths/parse-path path)]
         (doall (for [[type rule] (partition 2 rules)
                      :let [path (concat parsed-path (list type))
                            f (if (#{:index :children} type) (partial apply conj) conj)]]
                  (swap! *rules* update-in path f rule))))
       (catch js/Error e
         (.log js/console "add-err" e))))

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

(defn prior [target]
  (case target
    (data
      next-data
      prev-data) 'prev-data
    (root
      next-root
      prev-root) 'prev-root))

(defn condition-preds [target]
  (with-template-quotes
    {:create '(and (nil? ~(prior target)) (exists? ~target))
     :update '(and (not= nil ~(prior target))
                   (not= nil ~target)
                   (not= ~(prior target) ~target))
     :delete '(and (exists? ~(prior target)) (not (exists? ~target)))}))

(def cud-preds (condition-preds 'next-data))

(defn log [x] (prn x) x)

(defn compile
  ([rules] (compile rules [] 0))
  ([{:keys [create read update delete index write validate children] :as rules} path depth]
   (with-template-quotes
     (let [validate (cond-> validate
                            (seq children) (disj '(object? next-data)))]
       (merge
         (binding [*context* (context-with-path *context* path)]
           (cond-> {}
                   (seq read) (assoc ".read" (compile-expr {:mode :read}
                                                           '(and ~@read)))

                   (first (keep seq [write create update delete]))
                   (assoc ".write" (compile-expr {:mode :write}
                                                 (if (seq write)
                                                   (cond-> '(and ~@write)
                                                           (seq create) (append '(if ~(:create cud-preds) (and ~@create) true))
                                                           (seq update) (append '(if ~(:update cud-preds) (and ~@update) true))
                                                           (seq delete) (append '(if ~(:delete cud-preds) (and ~@delete) true)))
                                                   (cond->> 'false
                                                            (seq create) (append '(if ~(:create cud-preds) (and ~@create)))
                                                            (seq update) (append '(if ~(:update cud-preds) (and ~@update)))
                                                            (seq delete) (append '(if ~(:delete cud-preds) (and ~@delete)))))))
                   (seq index) (assoc ".indexOn" (vec index))
                   (or (seq validate)
                       (seq children)) (assoc ".validate"
                                              (compile-expr {:mode :write}
                                                            (cond-> '(and ~@validate)
                                                                    (seq children)
                                                                    (append '(contains-keys? next-data [~@children])))))))
         (reduce-kv (fn [m k v]
                        (assoc m (munge (cond->> k
                                                 (symbol? k) (str "$")))
                                 (compile v (conj path k) (inc depth))))
                    {}
                    (dissoc rules :create :read :update :delete :index :write :validate :children)))))))


(defn wrap-rule [rule]
  (with-template-quotes
    (if (symbol? rule) '(~rule next-data)
                       rule)))

(defn validate
  ([one-arg]
   (if (map? one-arg) (validate nil one-arg)
                      (validate one-arg nil)))
  ([root-rule child-rules]
   (when root-rule
     (add :validate (wrap-rule root-rule)))

   (when child-rules
     (doseq [[child-name rule] (seq child-rules)]
       (cond (map? rule)
             (path (munge (name child-name))
                   {:validate rule})
             (not (nil? rule))
             (add child-name :validate (wrap-rule rule))
             :else nil)
       (when (and (not (:optional (meta rule)))
                  (not (symbol? child-name)))
         (add :children [child-name])))

     (when (empty? (filter symbol? (keys child-rules)))
       (add 'other :validate false))

     (add :validate '(object? next-data)))))






'[

  (path ["users" uid]
        (let []
          {:write    ()
           :validate ()})
        ;; returning a map




        ;; example: ownership. a function returns a rule which is used in or as an expression.

        (f/defn owned?
                "Ensure data at this path is owned by an agent at owner-path."
                [owner-path]
                (rules [prev-data data]
                       (let [path [owner-path (get data "owner") (get data "id")]]
                         (and
                           (true? (root data path))
                           (true? (root prev-data path))))))

        (path ["users" user-id])
        (path ["cells" cell-id]
              (validate (owned? "users")))

        ;; destructuring of data arguments.
        ;;     implementation - either with spec, or a record type with `get` implemented.. the record
        ;;     knows its path, and adds it to the quoted symbol returned.
        (path ["docs" doc-id]
              (validate [prev-data {:keys [version-ts] :as doc}]
                        ;; version expands to (get next-data "version")
                        (and (= now version-ts)
                             (owned? "users"))))


        ;; example: timestamps
        ;;  here we have some rules that we want to re-use by merging them into other rules.
        ;;  we may need to have a special `rules` type which has its own `merge` implementation.
        (f/def timestamps
          (rules [prev {:keys [created-at] :as next}]
                 {:validate {:modified-at (= next now)}
                  :create   (= created-at now)
                  :update   (= (get prev "created-at") created-at)}))

        (defn use
          "merges a rule-map into the current path"
          [rule-map])

        (defmacro rules

          [bindings body])

        (defmacro authorize [bindings body]
          (use (rules ~bindings ~body)))

        (defmacro validate [bindings body]
          (use (rules ~bindings
                      {:validate ~body})))

        (defn owned?-2 [owner-path owner-key]
          (rules [prev-data data]
                 {:validate {:owner (exists? (root data [owner-path owner-key]))}}))


        (path [x]

              (use timestamps)
              (use (owned?-2 "users" "owner"))

              (authorize [prev-data data]
                         {:write  ()
                          :read   ()
                          :create ()})

              (validate {:title ()})

              ;; anonymous rules?
              (rule [prev-data data] (string? data))        ;; not sure what the point is



              (validate [prev-data data]
                        ;; `authorize` and `validate` also introduce scope, binding new symbols to 'next-data and 'prev-data
                        {:title ""})

              ;; pass multiple things to validate?
              (validate timestamps
                        ([prev-data data]
                          {:title (string? data)}))

              ;; design goal: inside `authorize` and `validate`, a map must be returned, but it could be:
              ;; note, problem if we put next-data and prev-data into arglists. then how do we share rules?
              (defkeys [prev next]
                       :timestamp/created-at (= next now)
                       :timestamp/modified-at (= next now))

              (keys {:req [:timestamp/created-at
                           :timestamp/modified-at]})

              (let [timestamps (rules [prev next]
                                      {:created-at  (= next now)
                                       :modified-at (= next now)})]
                (validate [prev-data data]
                          (merge {:title string?}
                                 timestamps)))

              ;; in some kind of expansion phase, we should be able to refer clojure functions en masse.
              ;; merge, assoc, disj, dissoc..

              (keys {:opt []
                     :req []})

              (index ["users"])

              ;;; OK...
              ;; - you can only use `rule` symbols inside a (rules [] ...) call.
              ;; - `authorize` and `validate` wrap a `rules` call.
              ;; still doesn't help figure out re-use (merge, assoc, etc.)
              ;; so far, `let` and `def` only help bind symbols to use *inside* a particular
              ;; rule, but do not help you compose rules / keys-with-rules.

              )

        ;; a `root` function operates on a data-snapshot
        (get-in (root data) [])
        ;; vs
        (get-in root [])
        (get-in prev-root [])
        ;; `root` could also use get-in
        (root data ["users"])
        (root prev-data ["users"])


        (path [x]
              {:authorize {:write  ()
                           :read   ()
                           :create ()}

               :validate  {:title ""}

               :keys      {:opt []
                           :req []}

               :index     ["users"]}

              )


        ;; map style
        (path [x]
              {:read    (root prev)
               :update? (root old)
               :valid?  true
               :write?  (= prev 10)
               :create? (= data 99)
               :delete? (= data 10)})


        ;; function style, no args:
        (path [x]

              (read (root prev))
              (update? (root old))
              (valid? true)
              (write? (= prev 10))
              (create? (= data 99))
              (delete? (= data 10))

              )

        (path [x]

              (write [old new])
              (read [data]
                    (root prev))
              (update? [old new]
                       (root old))
              (valid? [old new] true)

              (write?
                "Should not be able to write"
                [old new]
                (= prev 10))

              (create? [data]
                       (= data 99))
              (delete? [data]
                       (= data 10)))

        )

  ;; function style, take path as argument, works for threading

  (write? ["users"] [old new] (= old 10))

  (-> ["users"]
      (write? [old new] (= old 10))

      )

  (-> ["users"]
      (authorize {:write (= old 10)})
      (validate true))


  ;; recommended style: first build your own little library...

  (f/def admin-id "12345")
  (f/defn admin? [data] ...)

  ;; then use them in rules

  (path ["users" uid]
        )

  ]















