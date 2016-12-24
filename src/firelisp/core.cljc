(ns firelisp.core
  (:refer-clojure :exclude [defn defmacro fn])
  (:require
    [firelisp.template :refer [t] :include-macros true]
    [firelisp.specs :as specs :refer [get-arglists conf-meta parse-fn-args fn-wrap macro-wrap]]
    [firelisp.paths :refer [munge-sym refer-special-forms context-with-path]]
    [clojure.walk :as walk]
    [firelisp.convert :refer [convert-quotes]]
    [clojure.spec :as s :include-macros true]
    [clojure.zip :as z]
    [firelisp.post-zip :as w]
    [clojure.core :as core]
    [firelisp.next :as n :include-macros true]
    [firelisp.paths :as paths]))

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
  (assoc (specs/parse-fn-args specs/macro-wrap body)
    :type :macro))

(core/defmacro bind-anon-fn [type constructor body]
  (t (let [bindings# (:bindings firelisp.env/*context*)
           {f# :value type# :type} (~constructor ~body)]
       (doto (clojure.core/fn [& args#]
               (binding [firelisp.env/*context* (update firelisp.env/*context* :bindings merge bindings#)]
                 (firelisp.next/resolve-form (apply f# args#))))
         (aset "fire$type" type#)))))

(core/defmacro macro [& body]
  (t (firelisp.core/bind-anon-fn :macro firelisp.core/macro* ~body)))

(core/defmacro defmacro [& body]
  (t
    (let [{name# :name :as macro#} (firelisp.core/macro* ~body)]
      (firelisp.core/def* name# macro#))))

(core/defmacro fn* [body]
  (assoc (specs/parse-fn-args specs/fn-wrap body)
    :value (t (firelisp.standard-lib/make-fn (quote ~body)))
    :type :fn))

(core/defmacro fn [& body]
  (t (firelisp.core/bind-anon-fn :fn firelisp.core/fn* ~body)))

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
             (:create :read :update :delete :index :write :children) (t (~'firelisp.core/add ~type (firelisp.template/t ~rule)))
             (t (~'firelisp.core/path ~type ~rule)))))))

(core/defmacro path [path & body]
  (let [body (refer-special-forms body)
        body (cond-> body
                     (map? (first body)) (->> (drop 1)
                                              (cons (t (~'firelisp.core/authorize ~(first body))))))]

    (t
      (try (let [segments# (~'firelisp.paths/parse-path '~path)
                 leaf-rules# (binding [~'firelisp.env/*context* (firelisp.paths/context-with-path ~'firelisp.env/*context* '~path)
                                       ~'firelisp.env/*rules* (atom ~blank-rules)]
                               ~@body
                               @~'firelisp.env/*rules*)
                 rules# (->> leaf-rules#
                             (~'firelisp.core/update-in-map (some-> ~'firelisp.env/*rules* deref) segments# ~'firelisp.core/merge-rules)
                             (~'firelisp.core/filter-by-value #(not (and (set? %) (empty? %)))))]
             (some-> ~'firelisp.env/*rules*
                     (swap! ~'firelisp.core/merge-rules rules#))
             rules#)
           (catch ~'js/Error e#
             (~'.log ~'js/console "at error" e#))))))

(core/defmacro path** [path & body]
  (t (binding [~'firelisp.env/*context* (firelisp.paths/context-with-path ~'firelisp.env/*context* '~path)]
       (firelisp.compile/expand (quote ~(refer-special-forms body)))
       (reduce-kv (fn [m k v]
                      (cond-> m
                              (not= #{} v) (assoc k (if (seq? v) (into #{} (map firelisp.paths/clean-quotes v))
                                                                 v)))) {} @~'firelisp.env/*rules*))))

(core/defn collect-path [loc]
  (loop [loc loc
         path '()]
    (if-not (z/up loc)
      (vec path)
      (recur (z/up loc)
             (let [form (z/node loc)]
               (cond-> path
                       (and (seq? form) (= 'path* (first form))) (into (reverse (second form)))))))))

(core/defn path-forms [body]
  (let [loc (z/seq-zip (t (do (path* ~@body))))]
    (loop [loc (w/postorder-first loc)
           path-forms []]
      (if (z/end? loc)
        (t (do ~@path-forms))
        (let [form (z/node loc)
              collect? (and (seq? form) (= 'path* (first form)))]
          (recur (if collect? (-> (z/remove loc)
                                  (w/root)
                                  (w/postorder-first))
                              (w/postorder-next loc))
                 (cond-> path-forms
                         collect? (conj (t (firelisp.core/path** ~(collect-path loc) ~@(nnext form)))))))))))

(core/defmacro path* [& body]
  (t (binding [~'firelisp.env/*rules* (or ~'firelisp.env/*rules* (atom {}))]
       ~(path-forms body)
       @~'firelisp.env/*rules*))
  #_(t (do
         (println :path* (quote ~(path-forms (cons path body))))

         (binding [~'firelisp.env/*context* (firelisp.paths/context-with-path ~'firelisp.env/*context* '~path)
                   ~'firelisp.env/*rules* (or ~'firelisp.env/*rules* (atom ~blank-rules))]
           (firelisp.compile/expand (quote ~(refer-special-forms body)))
           (reduce-kv (fn [m k v] (cond-> m
                                          (seq v) (assoc k (into #{} (map firelisp.paths/clean-quotes v))))) {} @~'firelisp.env/*rules*)))))

(core/defmacro let [bindings & body]
  (t (binding [firelisp.env/*context* (firelisp.next/let-context-macro ~bindings)]
       (clojure.core/let ~(walk/postwalk-replace {'fn    'firelisp.core/fn
                                                  'fn*   'firelisp.core/fn
                                                  'macro 'firelisp.core/macro} bindings) ~@body))))




;; reduce number of macro entry-points, in preparation for unsplice of 'fn, 'fn*, 'defn, 'macro, 'defmacro.
;; then we have a 'whole' environment, w/ escape hatch to Clojure only via macro forms, and possibly (cljs* ).
;; - (path ..)
;; - (compile ..)