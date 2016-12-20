(ns firelisp.core
  (:refer-clojure :exclude [defn defmacro fn])
  (:require
    [firelisp.template :refer [t] :include-macros true]
    [firelisp.specs :refer [get-arglists conf-meta parse-fn-args fn-wrap macro-wrap]]
    [firelisp.paths :refer [munge-sym]]
    [clojure.walk :as walk]
    [firelisp.convert :refer [convert-quotes]]
    [clojure.spec :as s :include-macros true]
    [clojure.core :as core]))

(defonce ^:dynamic *defs* 'firelisp.env/*defs*)
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

(core/defmacro def* [name value]
  (t (swap! ~*defs* assoc ~name ~value)))

(core/defmacro def [name value]
  (t (swap! ~*defs* assoc (firelisp.paths/munge-sym ~name) {:name  ~name
                                                            :value ~value})))

(core/defmacro macro* [body]
  (parse-fn-args macro-wrap body))

(core/defmacro macro [& body]
  (t (get (firelisp.core/macro* ~body) :value)))

(core/defmacro defmacro [& body]
  (t
    (let [{name# :name :as macro#} (firelisp.core/macro* ~body)]
      (firelisp.core/def* name# macro#))))

(core/defmacro fn* [body]
  (parse-fn-args fn-wrap body))

(core/defmacro fn [& body]
  (t (get (firelisp.core/fn* ~body) :value)))

(core/defmacro defn [& body]
  (t (let [{name# :name :as fn#} (firelisp.core/fn* ~body)]
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
             ;; 2 - the return value is quoted (so we probably must use `t`
             '(defn my-func [a b]
                (let [argslist [a ~a
                                b ~b]]
                  (t (let ~argslist
                       (+ a b)))))
             )
          (= (my-func 1 2)
             '(let [a 1 b 2]
                (+ a b)))))



(core/defmacro authorize [m]
  (t (do
       ~@(for [[type rule] (seq m)]
           (case type
             :validate (t (~'firelisp.core/validate (firelisp.template/t ~rule)))
             (:create
               :read
               :update
               :delete

               :index
               :write
               :children) (t (~'firelisp.core/add ~type (firelisp.template/t ~rule)))
             (t (~'firelisp.core/at ~type ~rule)))))))

(core/defn quote-symbol [s]
  (if (symbol? s)
    (t '~s)
    s))

(core/defmacro at [path & body]
  (let [body (cond-> body
                     (map? (first body)) (->> (drop 1)
                                              (cons (t (~'firelisp.core/authorize ~(first body))))))
        path (mapv quote-symbol (if (vector? path) path [path]))]

    (t
      (try (let [segments# (~'firelisp.paths/parse-path ~path)
                 leaf-rules# (binding [~'firelisp.env/*context* (-> ~'firelisp.env/*context*
                                                                    (update :path into segments#)
                                                                    (update :bindings merge (firelisp.core/path-context segments#)))
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

(core/defmacro let [bindings body]
  (t (binding [firelisp.env/*context* (update firelisp.env/*context* :bindings merge ~(->> bindings
                                                                                           (walk/postwalk-replace {'fn    'firelisp.core/fn
                                                                                                                   'fn*   'firelisp.core/fn
                                                                                                                   'macro 'firelisp.core/macro})
                                                                                           (mapv #(if (symbol? %) (t (quote ~%)) %))
                                                                                           (apply hash-map)))]
       ~body)))




;; reduce number of macro entry-points, in preparation for unsplice of 'fn, 'fn*, 'defn, 'macro, 'defmacro.
;; then we have a 'whole' environment, w/ escape hatch to Clojure only via macro forms, and possibly (cljs* ).
;; - (at ..)
;; - (compile ..)