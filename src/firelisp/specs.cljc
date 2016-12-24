(ns firelisp.specs
  (:require [firelisp.template :refer [t] :include-macros true]
            [firelisp.paths :refer [munge-sym]]
            [clojure.spec :as s :include-macros true]))

(defn get-arglists [conf]
  (mapv #(get-in % [:args :args])
        (case (get-in conf [:bs 0])
          :arity-n (get-in conf [:bs 1 :bodies])
          :arity-1 [(get-in conf [:bs 1])])))

(defn conf-meta [{:keys [name docstring] :as conf}]
  (t {:name      '~name
      :docstring ~docstring
      :arglists  (quote ~(get-arglists conf))}))

(defn update-conf [{[arity] :bs :as conf} update-fn]
  (case arity
    :arity-1 (update-in conf [:bs 1] update-fn)
    :arity-n (update-in conf [:bs 1 :bodies] (fn [bodies]
                                               (map update-fn bodies)))))

(defn parse-fn-args
  ([body] (parse-fn-args identity body))
  ([update-f body]
   (let [conf (-> (s/conform :firelisp.specs/defn-args body)
                  (update-conf update-f))
         conf (cond-> conf
                      (contains? conf :name) (update :name munge-sym))
         new-args (s/unform :firelisp.specs/fn-args (dissoc conf :docstring))]
     (assoc (conf-meta conf) :value (t ~(cons 'cljs.core/fn new-args))))))

;loaded from gist: https://gist.github.com/viebel/ab64ed95820af42b366889a872dc28ac
;;;; destructure
(s/def ::local-name (s/and simple-symbol? #(not= '& %)))

(s/def ::binding-form
  (s/or :sym ::local-name
        :seq ::seq-binding-form
        :map ::map-binding-form))

;; sequential destructuring

(s/def ::seq-binding-form
  (s/and vector?
         (s/conformer identity vec)
         (s/cat :elems (s/* ::binding-form)
                :rest (s/? (s/cat :amp #{'&} :form ::binding-form))
                :as (s/? (s/cat :as #{:as} :sym ::local-name))
                )))

;; map destructuring

(s/def ::keys (s/coll-of ident? :kind vector?))
(s/def ::syms (s/coll-of symbol? :kind vector?))
(s/def ::strs (s/coll-of simple-symbol? :kind vector?))
(s/def ::or (s/map-of simple-symbol? any?))
(s/def ::as ::local-name)

(s/def ::map-special-binding
  (s/keys :opt-un [::as ::or ::keys ::syms ::strs]))

(s/def ::map-binding (s/tuple ::binding-form keyword?))

(s/def ::ns-keys
  (s/tuple
    (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
    (s/coll-of simple-symbol? :kind vector?)))

(s/def ::map-bindings
  (s/coll-of (s/or :mb ::map-binding
                   :nsk ::ns-keys
                   :msb (s/tuple #{:as :or :keys :syms :strs} any?)
                   )))

(s/def ::map-binding-form ::map-bindings #_(s/merge ::map-bindings ::map-special-binding))

;; bindings

(s/def ::binding (s/cat :binding ::binding-form :init-expr any?))
(s/def ::bindings (s/and vector? (s/* ::binding)))

;; defn, defn-, fn

(defn arg-list-unformer [a]
  (vec
    (if (and (coll? (last a)) (= '& (first (last a))))
      (concat (drop-last a) (last a))
      a)))

(s/def ::arg-list
  (s/and
    vector?
    (s/conformer identity arg-list-unformer)
    (s/cat :args (s/* ::binding-form)
           :varargs (s/? (s/cat :amp #{'&} :form ::binding-form)))))

(s/def ::args+body
  (s/cat :args ::arg-list
         :prepost (s/? map?)
         :body (s/* any?)))

(s/def ::defn-args
  (s/cat :name (s/? symbol?)
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                                    :attr (s/? map?)))))

(s/def ::fn-args
  (s/cat :name (s/? symbol?)
         :docstring (s/? string?)
         ;:meta (s/? map?)
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                                    :attr (s/? map?)))))

(defn macro-wrap
  [{body :body :as b}]
  (assoc b :body (t [(firelisp.common/with-template-quotes ~@body)])))

(defn get-entry [{[arity {bodies :bodies :as body}] :bs :as conf} num-args]
  (case arity
    :arity-1 body
    :arity-n (first (for [{{:keys [args varargs]} :args :as body} bodies
                          :let [a-count (count args)]
                          :when (or (= num-args a-count)
                                    (and (> num-args a-count)
                                         varargs))]
                      body))))

(defn get-args [conf n]
  (get-in (get-entry conf n) [:args :args]))

(defn destructure-arg [arg]
  (let [results (atom [])]
    ((fn destructure
       ([arg] (destructure [] arg))
       ([path arg]

        (let [[type val path-segment] arg
              path (cond-> path
                           path-segment (conj path-segment))]
          (case type
            :sym (swap! results conj {:name val
                                      :path path} #_(t (get-in ~arg-name ~(conj path val))))
            :map (mapv (partial destructure path) val)
            :mb (destructure path (cond-> (first val)
                                          (second val) (conj (second val))))
            :msb (destructure path val)
            :seq (do (doall (map-indexed #(destructure (conj path %1) %2) (:elems val)))
                     (when-let [sym (get-in val [:as :sym])]
                       (destructure path [:sym sym])))
            :keys (mapv #(destructure (conj path (keyword %)) [:sym %]) val)
            :as (destructure path [:sym val])
            (do (println :what-to-do! type val path-segment)
                (swap! results conj [:unknown val])))))) arg)
    @results))

(defn destructure-arglist [{:keys [args varargs]}]
  (cond-> (mapv destructure-arg args)
          varargs (conj (destructure-arg (:form varargs)))))


(defn binding-assignments [names-form value]
  (mapcat (fn [{:keys [name path]}]
            (let [path (mapv clojure.core/name path)]
              [name (let [n (count path)]
                      (cond (= n 0) value
                            (= n 1) (t (get ~value ~(first path)))
                            :else (t (get-in ~value ~path))))])) names-form))

(defn merge-bindings [arg-entry args]
  (let [args (if (:varargs arg-entry) (let [[fixed-args rest-args] (split-at (count (:args arg-entry)) args)]
                                        (concat fixed-args (list rest-args))) args)]
    (vec (mapcat binding-assignments (destructure-arglist arg-entry) args))))


(defn fn-wrap
  [{body :body {[& arglist] :args} :args :as b}]
  (assoc b :body
           (t
             [(list 'let (vec (interleave ~(t (quote ~arglist)) ~(vec arglist))) (quote ~@body))])))