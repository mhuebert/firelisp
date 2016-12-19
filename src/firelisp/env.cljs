(ns firelisp.env)


(def ^:dynamic *rules* nil)
(def ^:dynamic *path* [])
(defonce ^:dynamic *defs* (atom {}))
(defonce terminal-defs (atom {}))