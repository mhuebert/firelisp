(ns firelisp.env
  (:require [clojure.string :as string]
            [firelisp.paths :as paths]))


(def ^:dynamic *rules* nil)
(def ^:dynamic *path* [])
(def ^:dynamic *context* {:path    []
                          :bindings {}})
(defonce ^:dynamic *defs* (atom {}))
(defonce terminal-defs (atom {}))

(defn resolve-sym [sym]
  (when-let [sym (some-> sym paths/elide-core)]
    (or
      (get-in @*defs* [(paths/munge-sym sym) :value])
      (get-in *context* [:bindings (paths/munge-sym sym)]))))