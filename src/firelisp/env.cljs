(ns firelisp.env
  (:require [clojure.string :as string]
            ))


(def ^:dynamic *rules* nil)
(def ^:dynamic *path* [])
(def ^:dynamic *context* {:path    []
                          :bindings {}})
(defonce ^:dynamic *defs* (atom {}))
(defonce terminal-defs (atom {}))


(defn as-symbol
  "Unquote symbols"
  [n]
  (if (seq? n) (second n) n))

(defn munge-sym [sym]
  (when-let [sym (as-symbol sym)]
    (-> sym
        (str)
        (string/replace "/" "__")
        symbol)))

(defn resolve-sym [sym]
  (when-let [sym (and (symbol? sym)
                      (cond-> sym
                              (= (namespace sym) "firelisp.core") (name)))]
    (or
      (get-in @*defs* [(munge-sym sym) :value])
      (get-in *context* [:bindings (munge-sym sym)]))))