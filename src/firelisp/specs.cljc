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
   (let [conf (-> (s/conform :cljs.core/defn-args body)
                  (update-conf update-f)
                  (update :name munge-sym))
         new-args (s/unform :cljs.core/fn-args conf)]
     (assoc (conf-meta conf) :value (t ~(cons 'cljs.core/fn new-args))))))

;loaded from gist: https://gist.github.com/viebel/ab64ed95820af42b366889a872dc28ac
;;;; destructure

(s/def ::local-name (s/and symbol? #(not= '& %)))

(s/def ::binding-form any?)
(s/def ::binding (s/cat :binding ::binding-form :init-expr any?))
(s/def ::bindings (s/and vector? (s/* ::binding)))

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

(s/def :cljs.core/defn-args
  (s/cat :name (s/? symbol?)
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec
                                                   ::args+body))
                                    :attr (s/? map?)))))

(s/def :cljs.core/fn-args
  (s/cat :name symbol?
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec
                                                   ::args+body))
                                    :attr (s/? map?)))))

(defn fn-wrap
  [{body :body {[& arglist] :args} :args :as b}]
  (assoc b :body
           (t
             [(list 'let (vec (interleave ~(t (quote ~arglist)) ~(vec arglist))) (quote ~@body))])))

(defn macro-wrap
  [{body :body :as b}]
  (assoc b :body (t [(firelisp.common/with-template-quotes ~@body)])))