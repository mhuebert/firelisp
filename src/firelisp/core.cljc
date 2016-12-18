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
    [clojure.string :as string]
    [clojure.spec :as s]])))

(def *defs* 'firelisp.env/*defs*)
(def core-fn #?(:cljs 'cljs.core/fn
                :clj  'clojure.core/fn))

(core/defn munge-name [b]
  (update b :name #(-> (str %)
                       (string/replace "/" "__")
                       symbol)  #_(comp symbol munge str)))

(core/defn update-conf [{[arity] :bs :as conf} update-fn]
  (case arity
    :arity-1 (update-in conf [:bs 1] update-fn)
    :arity-n (update-in conf [:bs 1 :bodies] (core/fn [bodies]
                                               (map update-fn bodies)))))

(core/defn macro-wrap
  [{body :body :as b}]
  (assoc b :body (template [(firelisp.common/with-template-quotes ~@body)])))

(core/defmacro macro [& body]
  (let [{:keys [name docstring] :as conf} (-> (s/conform :cljs.core/defn-args body)
                                              (update-conf macro-wrap)
                                              (munge-name))
        new-args (s/unform :cljs.core/fn-args conf)]
    (template (do (println :macro (quote ~conf))
                  {:name      '~name
                   :docstring ~docstring
                   :fn        ~(cons 'cljs.core/fn new-args)}))))

(core/defmacro defmacro [name & body]
  (template
    (let [{name# :name :as macro#} (firelisp.core/macro ~name ~@body)]
      (swap! ~*defs* assoc name# macro#))))

(core/defn fn-wrap
  [{body :body {[& arglist] :args} :args :as b}]
  (assoc b :body
           (template
             [(list 'let (vec (interleave ~(template (quote ~arglist)) ~(vec arglist))) (quote ~@body))])))

(core/defmacro fn [& body]
  (let [{:keys [name docstring] :as conf} (-> (s/conform :cljs.core/defn-args body)
                                              (update-conf fn-wrap)
                                              (munge-name))
        new-args (s/unform :cljs.core/defn-args conf)]
    (template {:name      (quote ~name)
               :docstring ~docstring
               :fn        ~(cons 'cljs.core/fn new-args)})))

(core/defmacro defn [& body]
  (template
    (let [{name# :name :as fn#} (firelisp.core/fn ~@body)]
      (swap! ~*defs* assoc name# fn#))))

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




