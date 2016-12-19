(ns firelisp.specs
  (:require [firelisp.template :refer [template] :include-macros true]
            [clojure.spec :as s :include-macros true]))

(defn get-arglists [conf]
  (mapv #(get-in % [:args :args])
        (case (get-in conf [:bs 0])
          :arity-n (get-in conf [:bs 1 :bodies])
          :arity-1 [(get-in conf [:bs 1])])))

(defn conf-meta [{:keys [name docstring] :as conf}]
  (template {:name      '~name
             :docstring ~docstring
             :arglists  (quote ~(get-arglists conf))}))

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
  (s/cat :name symbol?
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