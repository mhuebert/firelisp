(ns firelisp.ruleset
  (:require
    [firelisp.backtick]
    [firelisp.common :refer [convert-quotes]]))

(defmacro with-template-quotes [& body]
  (firelisp.backtick/template
    (do ~@(convert-quotes body))))

(defmacro rulefn [name & body]
  (with-template-quotes
    (let [body (cond-> body
                       (string? (first body)) rest)]
      '(swap! firelisp.ruleset/*fire-fns*
              assoc (quote ~name) (firelisp.ruleset/fire-fn ~@(cons name body))))))

(defmacro fire-fn [& body]
  (with-template-quotes
    '(firelisp.ruleset/with-template-quotes
       (fn ~@body))))

(def blank-rules {:read     #{}
                  :create   #{}
                  :update   #{}
                  :delete   #{}
                  :validate #{}
                  :write    #{}
                  :index    #{}
                  :children #{}})

(defmacro add
  [path & rules]
  (if (odd? (count rules))
    (with-template-quotes
      '(firelisp.ruleset/add "" ~@(cons path rules)))
    (with-template-quotes
      '(let [parsed-path (firelisp.paths/parse-path ~path)]
         ~@(for [[type rule] (partition 2 rules)]
             '(let [f (if (#{:index :children} ~type) (partial apply conj) conj)]
                (swap! firelisp.ruleset/*rules* update-in (concat (map str parsed-path) (list ~type)) f ~rule)))))))

(defmacro authorize [m]
  (with-template-quotes
    '(do
       ~@(for [[type rule] (seq m)]
           (case type
             :validate '(firelisp.ruleset/validate ~rule)
             (:create
               :read
               :update
               :delete
               :index
               :write
               :children) '(firelisp.ruleset/add ~type ~rule)
             '(firelisp.ruleset/at ~type ~rule))))))

(defmacro at [path & body]
  (with-template-quotes
    (let [body (cond-> body
                       (map? (first body)) (->> (drop 1)
                                                (cons '(firelisp.ruleset/authorize ~(first body)))))]
      '(let [segments# (map str (firelisp.paths/parse-path ~path))
             leaf-rules# (binding [firelisp.ruleset/*path* (apply conj firelisp.ruleset/*path* segments#)
                                   firelisp.ruleset/*rules* (atom ~blank-rules)]
                           ~@(firelisp.common/convert-quotes body)
                           @firelisp.ruleset/*rules*)
             rules# (->> leaf-rules#
                         (firelisp.ruleset/update-in-map (some-> firelisp.ruleset/*rules* deref) segments# firelisp.ruleset/merge-rules)
                         (firelisp.ruleset/filter-by-value #(not (and (set? %) (empty? %)))))]
         (some-> firelisp.ruleset/*rules*
                 (swap! firelisp.ruleset/merge-rules rules#))
         rules#))))