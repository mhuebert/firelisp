(ns firelisp.core
  (:refer-clojure :exclude [defn defmacro fn])
  (:require
    [firelisp.template :refer [template] :include-macros true]
    [firelisp.specs :refer [get-arglists conf-meta]]
    [firelisp.paths :refer [munge-sym]]
    [firelisp.convert :refer [convert-quotes]]
    [clojure.spec :as s :include-macros true]
    [clojure.core :as core]))

(defonce *defs* 'firelisp.env/*defs*)
(defonce core-fn #?(:cljs 'cljs.core/fn
                    :clj  'clojure.core/fn))
(defonce blank-rules
         {:validate #{}

          :create   #{}
          :read     #{}
          :update   #{}
          :delete   #{}

          :write    #{}
          :index    #{}
          :children #{}})



(core/defmacro def [name value]
  (template (swap! ~*defs* assoc (firelisp.paths/munge-sym ~name) {:name  ~name
                                                                   :value ~value})))

(core/defmacro def* [name value]
  (template (swap! ~*defs* assoc ~name ~value)))

(core/defn update-conf [{[arity] :bs :as conf} update-fn]
  (case arity
    :arity-1 (update-in conf [:bs 1] update-fn)
    :arity-n (update-in conf [:bs 1 :bodies] (core/fn [bodies]
                                               (map update-fn bodies)))))

(core/defn macro-wrap
  [{body :body :as b}]
  (assoc b :body (template [(firelisp.common/with-template-quotes ~@body)])))

(core/defmacro macro [& body]
  (let [conf (-> (s/conform :cljs.core/defn-args body)
                 (update-conf macro-wrap)
                 (update :name munge-sym))
        new-args (s/unform :cljs.core/fn-args conf)]
    (assoc (conf-meta conf) :value (template ~(cons 'cljs.core/fn new-args)))))

(core/defmacro defmacro [name & body]
  (template
    (let [{name# :name :as macro#} (firelisp.core/macro ~name ~@body)]
      (firelisp.core/def* name# macro#))))

(core/defn fn-wrap
  [{body :body {[& arglist] :args} :args :as b}]
  (assoc b :body
           (template
             [(list 'let (vec (interleave ~(template (quote ~arglist)) ~(vec arglist))) (quote ~@body))])))

(core/defmacro fn [& body]
  (let [conf (-> (s/conform :cljs.core/defn-args body)
                 (update-conf fn-wrap)
                 (update :name munge-sym))
        new-args (s/unform :cljs.core/defn-args conf)]
    (assoc (conf-meta conf) :value (template ~(cons 'cljs.core/fn new-args)))))

(core/defmacro defn [& body]
  (template
    (let [{name# :name :as fn#} (firelisp.core/fn ~@body)]
      (firelisp.core/def* name# fn#))))

;; X docstrings
;; - namespaces for defs?
;; - show current ns at top of screen. show recently used & available ns's. click on ns to change.

(comment
  (assert (= (macroexpand-1 '(defn my-func [a b]
                               (+ a b)))
             ;; return a function that accepts arguments & quoted forms,
             ;; returns a form with two modifications,
             ;; 1 - (let [arg-a-symbol ~arg-a-value, arg-b-symbol ~arg-b-value] ...)
             ;; 2 - the return value is quoted (so we probably must use `template`
             '(defn my-func [a b]
                (let [argslist [a ~a
                                b ~b]]
                  (template
                    (let ~argslist
                      (+ a b)))))
             )
          (= (my-func 1 2)
             '(let [a 1 b 2]
                (+ a b)))))



(core/defmacro authorize [m]
  (template (do
              ~@(for [[type rule] (seq m)]
                  (case type
                    :validate (template (~'firelisp.core/validate (firelisp.template/template ~rule)))
                    (:create
                      :read
                      :update
                      :delete

                      :index
                      :write
                      :children) (template (~'firelisp.core/add ~type (firelisp.template/template ~rule)))
                    (template (~'firelisp.core/at ~type ~rule)))))))

(core/defn quote-symbol [s]
  (if (symbol? s)
    (template '~s)
    s))

(core/defmacro at [path & body]
  (let [body (cond-> body
                     (map? (first body)) (->> (drop 1)
                                              (cons (template (~'firelisp.core/authorize ~(first body))))))
        path (mapv quote-symbol (if (vector? path) path [path]))]

    (template
      (try (let [segments# (~'firelisp.paths/parse-path ~path)
                 leaf-rules# (binding [~'firelisp.env/*path* (apply conj ~'firelisp.env/*path* segments#)
                                       ~'firelisp.env/*rules* (atom ~blank-rules)]
                               ~@(convert-quotes body)
                               @~'firelisp.env/*rules*)
                 rules# (->> leaf-rules#
                             (~'firelisp.core/update-in-map (some-> ~'firelisp.env/*rules* deref) segments# ~'firelisp.core/merge-rules)
                             (~'firelisp.core/filter-by-value #(not (and (set? %) (empty? %)))))]
             (some-> ~'firelisp.env/*rules*
                     (swap! ~'firelisp.core/merge-rules rules#))
             rules#)
           (catch ~'js/Error e#
             (~'.log ~'js/console "at error" e#))))))




