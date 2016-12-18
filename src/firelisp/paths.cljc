(ns firelisp.paths
  (:require [clojure.string :as string]
            [firelisp.template :refer [template] :include-macros true]))

(defn split-path [s]
  (let [trimmed-s (second (re-find #"^/?(.*)/?$" s))]
    (if (= trimmed-s "") []
                         (as-> trimmed-s s
                               (string/split s #"/")
                               (map #(if-let [variable (or (second (re-find #"^\{(.*)\}$" %))
                                                           (second (re-find #"^\$(.*)$" %)))]
                                       (symbol (munge (str variable)))
                                       %) s)))))

(defn throw-duplicate-path-variables [segments]
  (let [symbols (filter symbol? segments)]
    (when-not (apply = (map count [symbols (distinct symbols)]))
      (throw (str "Duplicate use of path variables " (apply str (interpose ", " symbols))))))
  segments)

(defn parse-path [segments]

  (throw-duplicate-path-variables
    (loop [results []
           [next-segment & remaining] (cond-> segments
                                              (string? segments) split-path
                                              (keyword? segments) ((comp list name))
                                              (symbol? segments) (vector))]
      (if-not next-segment
        results
        (recur (cond (string? next-segment) (apply conj (cons results (split-path next-segment)))
                     (keyword? next-segment) (conj results (name next-segment))
                     (symbol? next-segment) (conj results next-segment)
                     :else (conj results next-segment))
               remaining)))))