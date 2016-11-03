(ns firelisp.rules
  (:require
    [firelisp.paths]
    [firelisp.common :refer [convert-quotes]]))


(defmacro rulefn* [& body]
  `(firelisp.common/with-template-quotes
     (fn ~@body)))

(defmacro rulefn [name & body]
  (let [body (cond-> body
                     (string? (first body)) rest)]
    `(swap! ~'firelisp.compile/*rule-fns*
            assoc (quote ~name) (firelisp.rules/rulefn* ~@(cons name body)))))

(def blank-rules {:read     #{}
                  :create   #{}
                  :update   #{}
                  :delete   #{}
                  :validate #{}
                  :write    #{}
                  :index    #{}
                  :children #{}})

(defmacro authorize [m]
  `(do
     ~@(for [[type rule] (seq m)]
         (case type
           :validate `(~'firelisp.rules/validate ~rule)
           (:create
             :read
             :update
             :delete
             :index
             :write
             :children) `(~'firelisp.rules/add ~type ~rule)
           `(~'firelisp.rules/at ~type ~rule)))))

(defmacro at [path & body]
  (let [body (cond-> body
                     (map? (first body)) (->> (drop 1)
                                              (cons `(~'firelisp.rules/authorize ~(first body)))))]
    `(try (let [segments# (map str (~'firelisp.paths/parse-path ~path))
                leaf-rules# (binding [~'firelisp.compile/*path* (apply conj ~'firelisp.compile/*path* segments#)
                                      ~'firelisp.rules/*rules* (atom ~blank-rules)]
                              ~@(convert-quotes body)
                              @~'firelisp.rules/*rules*)
                rules# (->> leaf-rules#
                            (~'firelisp.rules/update-in-map (some-> ~'firelisp.rules/*rules* deref) segments# ~'firelisp.rules/merge-rules)
                            (~'firelisp.rules/filter-by-value #(not (and (set? %) (empty? %)))))]
            (some-> ~'firelisp.rules/*rules*
                    (swap! ~'firelisp.rules/merge-rules rules#))
            rules#)
          (catch ~'js/Error e#
            (~'.log ~'js/console "at error" e#)))))