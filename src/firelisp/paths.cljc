(ns firelisp.paths
  (:require [clojure.string :as string]
            [firelisp.template :refer [t] :include-macros true]
            [clojure.walk :as walk]))

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

(defn quote-symbol [s]
  (if (symbol? s)
    (t '~s)
    s))

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


(defn dequote-symbols
  "Unquote symbols"
  [n]
  (if (seq? n) (second n) n))

(defn as-symbol [sym]
  (when (symbol? (dequote-symbols sym)) sym))

(defn elide-core
  "Elide namespace of core symbols"
  [sym]
  (some-> (as-symbol sym)
          (cond-> (= (namespace sym) "firelisp.core") (name))
          (symbol)))

(defn munge-sym [sym]
  (when-let [sym (dequote-symbols sym)]
    (-> sym
        (str)
        (string/replace "/" "__")
        symbol)))

(defn clean-quotes [s]
  (if (and (list? s) (= 'firelisp.template/t (first s)))
    (second s) s))

(defn convert-quotes [form]
  (walk/postwalk (fn [x]
                   (if (and (seq? x)
                            (= (first x) 'quote)
                            (not (and (symbol? (second x))
                                      (= 2 (count x)))))
                     (cons 'firelisp.template/t (rest x))
                     x)) form))

(defn refer-special-forms [body]
  (walk/postwalk (fn [x]
                   (if (seq? x)
                     (case (first x)
                       quote (cons 'firelisp.template/t (rest x))
                       fn (cons 'firelisp.core/fn (rest x))
                       fn* (cons 'firelisp.core/fn (rest x))
                       macro (cons 'firelisp.core/macro (rest x))
                       let (cons 'firelisp.core/let (rest x))
                       x)
                     x))
                 body))

(defn context-with-path
  [ctx path]
  (let [segments (parse-path path)]
    (-> ctx
        (update :path into segments)
        (update :bindings merge (reduce (fn [m k]
                                          (cond-> m
                                                  (symbol? k) (assoc k {:name  k
                                                                        :value (symbol (str "$" k))
                                                                        :type  :path-variable}))) {} path)))))