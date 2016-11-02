(ns firelisp.rules
  (:require
    [firelisp.backtick]
    [firelisp.common :refer [convert-quotes]]))

(defmacro with-template-quotes [& body]
  (firelisp.backtick/template
    (do ~@(convert-quotes body))))

(defmacro rulefn* [& body]
  (with-template-quotes
    '(firelisp.rules/with-template-quotes
       (fn ~@body))))

(defmacro rulefn [name & body]
  (with-template-quotes
    (let [body (cond-> body
                       (string? (first body)) rest)]
      '(swap! firelisp.compile/*rule-fns*
              assoc (quote ~name) (firelisp.rules/rulefn* ~@(cons name body))))))

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
      '(firelisp.rules/add "" ~@(cons path rules)))
    (with-template-quotes
      '(let [parsed-path (firelisp.paths/parse-path ~path)]
         ~@(for [[type rule] (partition 2 rules)]
             '(let [f (if (#{:index :children} ~type) (partial apply conj) conj)]
                (swap! firelisp.rules/*rules* update-in (concat (map str parsed-path) (list ~type)) f ~rule)))))))

(defmacro authorize [m]
  (with-template-quotes
    '(do
       ~@(for [[type rule] (seq m)]
           (case type
             :validate '(firelisp.rules/validate ~rule)
             (:create
               :read
               :update
               :delete
               :index
               :write
               :children) '(firelisp.rules/add ~type ~rule)
             '(firelisp.rules/at ~type ~rule))))))

(defmacro at [path & body]
  (with-template-quotes
    (let [body (cond-> body
                       (map? (first body)) (->> (drop 1)
                                                (cons '(firelisp.rules/authorize ~(first body)))))]
      '(let [segments# (map str (firelisp.paths/parse-path ~path))
             leaf-rules# (binding [firelisp.compile/*path* (apply conj firelisp.compile/*path* segments#)
                                   firelisp.rules/*rules* (atom ~blank-rules)]
                           ~@(firelisp.common/convert-quotes body)
                           @firelisp.rules/*rules*)
             rules# (->> leaf-rules#
                         (firelisp.rules/update-in-map (some-> firelisp.rules/*rules* deref) segments# firelisp.rules/merge-rules)
                         (firelisp.rules/filter-by-value #(not (and (set? %) (empty? %)))))]
         (some-> firelisp.rules/*rules*
                 (swap! firelisp.rules/merge-rules rules#))
         rules#))))