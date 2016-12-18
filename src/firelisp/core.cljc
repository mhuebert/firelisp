(ns firelisp.core
  (:refer-clojure :exclude [defn defmacro fn])
  (:require
    [firelisp.template :refer [template] :include-macros true]
    [firelisp.specs]
    [firelisp.paths]
    [firelisp.convert :refer [convert-quotes]]
    #?@(:cljs [[cljs.core :as core]
               [cljs.spec :as s :include-macros true]]
        :clj  [
    [clojure.core :as core]
    [clojure.spec :as s]])))

(def *defs* 'firelisp.env/*defs*)
(def core-fn #?(:cljs 'cljs.core/fn
                :clj  'clojure.core/fn))

(core/defn update-conf [{[arity] :bs :as conf} update-fn]
  (case arity
    :arity-1 (update-in conf [:bs 1] update-fn)
    :arity-n (update-in conf [:bs 1 :bodies] (core/fn [bodies]
                                               (map update-fn bodies)))))

(core/defn template-wrap
  [{body :body {[& arglist] :args} :args :as b}]
  (assoc b :body
           (template
             [(list 'let (vec (interleave ~(template (quote ~arglist)) ~(vec arglist))) (quote ~@body))])))

(core/defn macro-wrap
  [{body :body {[& arglist] :args} :args :as b}]
  (assoc b :body
           (template [(firelisp.common/with-template-quotes ~@body)])
           #_(template
               [(list 'let (vec (interleave ~(template (quote ~arglist)) ~(vec arglist))) (quote ~@body))])))

(core/defmacro macro* [& body]
  (let [{:keys [name docstring] :as conf} (s/conform :cljs.core/defn-args body)
        new-conf (-> (update-conf conf macro-wrap)
                     (dissoc :name :docstring))
        new-args (s/unform :cljs.core/fn-args new-conf)]
    (template {:name      '~name
               :docstring ~docstring
               :fn        ~(cons 'cljs.core/fn new-args)})))

(core/defmacro macro [& body]
  (template (firelisp.common/with-template-quotes
              (~core-fn ~@body))))

(core/defmacro defmacro [name & body]
  (template
    (let [{name# :name fn# :fn :as f#} (firelisp.core/macro* ~name ~@body)]
      (swap! ~*defs* assoc name# fn#))))

(core/defmacro fn [& body]
  (let [conf (s/conform :cljs.core/defn-args body)
        {:keys [name docstring] :as new-conf} (update-conf conf template-wrap)
        new-args (s/unform :cljs.core/defn-args new-conf)]
    (template {:name      (quote ~name)
               :docstring ~docstring
               :fn        ~(cons 'cljs.core/fn new-args)})))

(core/defmacro defn [& body]
  (template
    (let [{name# :name fn# :fn} (firelisp.core/fn ~@body)]
      (swap! ~*defs* assoc name# fn#
             #_{:name      (quote ~name)
                :docstring ~docstring
                :fn        ~(cons 'cljs.core/fn new-args)}))))

;; - docstrings
;; - namespaces (firelisp.defs.core, firelisp.defs.*)
;; idea - evaluate Firelisp with a different `resolve-symbol` binding; could
;;        i use existing cljs namespace stuff but restrict to fire-fns?

;; - firelisp fns in a special namespace
;; - exclude all clojure core functions that overlap
;; - ^^ problem with this is, what about when you want to 'escape' to clojure?
;;
;; show current ns at top of screen. show recently used & available ns's. click on ns to change.
;; manipulate resolve-var so that macro namespaces behave seamlessly with normal namespaces.

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

(def blank-rules
  {:validate #{}

   :create   #{}
   :read     #{}
   :update   #{}
   :delete   #{}

   :write    #{}
   :index    #{}
   :children #{}})

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
                 leaf-rules# (binding [~'firelisp.compile/*path* (apply conj ~'firelisp.compile/*path* segments#)
                                       ~'firelisp.core/*rules* (atom ~blank-rules)]
                               ~@(convert-quotes body)
                               @~'firelisp.core/*rules*)
                 rules# (->> leaf-rules#
                             (~'firelisp.core/update-in-map (some-> ~'firelisp.core/*rules* deref) segments# ~'firelisp.core/merge-rules)
                             (~'firelisp.core/filter-by-value #(not (and (set? %) (empty? %)))))]
             (some-> ~'firelisp.core/*rules*
                     (swap! ~'firelisp.core/merge-rules rules#))
             rules#)
           (catch ~'js/Error e#
             (~'.log ~'js/console "at error" e#))))))




