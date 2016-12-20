(ns firelisp.env)


(def ^:dynamic *rules* nil)
(def ^:dynamic *path* [])
(def ^:dynamic *context* {:path    []
                          :symbols {}})
(defonce ^:dynamic *defs* (atom {}))
(defonce terminal-defs (atom {}))

