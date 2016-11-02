(ns firelisp.compile
  (:require [clojure.string :as string]
            [firelisp.walk :refer [postorder-replace]]))

(def ^:dynamic *path* [])
(def ^:dynamic *rule-fns* (atom {}))

(def terminal-forms '#{and or = not= + - * / % > < >= <= do prior not if exists? number? string? boolean? has-children? object? parent child has-child? upper-case lower-case contains? starts-with? ends-with? matches? replace length})

(defn expand-1 [fns form]
  (postorder-replace
    (fn [expr]
      (if (seq? expr)
        (if-let [operator (some->> (first expr)
                                   (get fns))]
          (apply operator (rest expr))
          (do
            (when-not (contains? terminal-forms (first expr)) (prn "Operator not found: " (first expr)))
            expr))
        expr)) form))

(defn expand
  [expr]
  (loop [current-expr expr
         count 0]
    (let [next-expr (expand-1 @*rule-fns* current-expr)]
      (when (> count 100)
        (throw "Expand-fns probably in a loop, iterated 100 times"))
      (if (= next-expr current-expr)
        next-expr
        (recur next-expr
               (inc count))))))

(defn wrap [s]
  (str "(" s ")"))

(defmulti emit :type)

(defn infix [js-operator {:keys [args]}]
  (if (= 1 (count args))
    (emit (first args))
    (wrap (apply str (interpose (str " " js-operator " ") (map emit args))))))

(defn method
  ([js-operator opts] (method js-operator opts ", "))
  ([js-operator {:keys [args]} separator]
   (str (emit (first args)) "." js-operator (wrap (string/join separator (map emit (rest args)))))))

(defmethod emit :vector
  [{:keys [args]}]
  (str "[" (string/join ", " (map emit args)) "]"))

(defmethod emit :list
  [{:keys [operator args prior as-snapshot?] :as n}]
  (let [arg-count (count args)]
    (case operator
      and (infix "&&" n)
      or (infix "||" n)
      = (infix "===" n)
      not= (infix "!==" n)
      + (infix "+" n)
      - (infix "-" n)
      * (infix "*" n)
      / (infix "/" n)
      % (infix "%" n)
      > (infix ">" n)
      < (infix "<" n)
      >= (infix ">=" n)
      <= (infix "<=" n)
      do (emit (last args))
      prior (do (assert (= 1 arg-count))
                (emit (last args)))
      not (do (assert (= 1 arg-count))
              (str "!" (emit (last args))))
      if (do (assert (#{2 3} arg-count))
             (let [[pred e1 e2] (map (partial emit) args)]
               (wrap (str pred " ? " e1 " : " e2))))

      ;; spanshot methods
      exists? (method "exists" n)
      number? (method "isNumber" n)
      string? (method "isString" n)
      boolean? (method "isBoolean" n)

      has-children? (method "hasChildren" n)
      object? (method "hasChildren" n)

      parent (cond-> (method "parent" n)
                     (not as-snapshot?) (str ".val()"))
      child (cond-> (method "child" n " + '/' + ")
                    (not as-snapshot?) (str ".val()"))
      has-child? (method "hasChild" n)

      ;; string methods
      upper-case (method "toUpperCase" n)
      lower-case (method "toLowerCase" n)
      contains? (method "contains" n)
      starts-with? (method "beginsWith" n)
      ends-with? (method "endsWith" n)
      matches? (method "matches" n)
      replace (method "replace" n)
      length (str (emit (first args)) ".length"))))

(defn atom-type [form]
  (case form
    data :snapshot
    root :snapshot
    nil :nil
    (= form 'now) :timestamp
    (cond (number? form) :number
          (symbol? form) :symbol
          (string? form) :string
          (regexp? form) :regexp
          (boolean? form) :boolean
          (keyword? form) :keyword
          :else :other)))

(defmethod emit :atom
  [{:keys [value mode as-snapshot? prior atom-type path]}]
  (case atom-type
    :nil "null"
    :timestamp "now"
    (:number
      :regexp) value
    (:string
      :keyword) (str "'" (name value) "'")
    (:boolean
      :symbol) (str value)
    :snapshot (cond-> (case value
                        data (if (or (= mode :read) prior) "data" "newData")
                        root (if (or (= mode :read) prior) "root" (str "newData"
                                                                       (apply str (take (count *path*)
                                                                                        (repeat ".parent()"))))))
                      (not as-snapshot?) (str ".val()"))
    (str "<" (when prior "prior: ") value ">")))

(defn node-type [form]
  (cond (vector? form) :vector
        (seq? form) :list
        :else :atom))

(defmulti node (fn [_ form]
                 (node-type form)))

(defmethod node :vector
  [opts form]
  (merge (dissoc opts :as-snapshot?)
         {:type :vector
          :path *path*
          :args (map (partial node (assoc opts :coerce-to-val true)) (seq form))}))

(defn snapshot-method? [sym]
  (contains? #{'has-children?
               'exists?
               'number?
               'string?
               'boolean?
               'object?
               'parent
               'child
               'has-child?} sym))

(defn no-op? [form]
  (and (seq? form)
       (#{'prior 'do} (first form))))

(defmethod node :list
  [opts form]
  (let [no-op? (no-op? form)
        first-arg-as-snapshot? (or (snapshot-method? (first form))
                                   (and (:as-snapshot? opts) no-op?))
        opts (cond-> opts (= 'prior (first form)) (assoc :prior true))
        args (cons (node (cond-> opts
                                 first-arg-as-snapshot? (assoc :as-snapshot? true)) (first (rest form)))
                   (map (partial node opts) (drop 2 form)))]
    (if no-op?
      (node opts (last form))
      (merge {:type     :list
              :path     *path*
              :operator (first form)
              :args     args}
             opts))))

(defmethod node :atom
  [opts form]
  (let [atom-type (atom-type form)]
    (when (and (= atom-type :symbol)
               (= \$ (first (name form)))
               (not (contains? (set (map str *path*)) (munge (name form)))))
      (throw (js/Error (str "Path variable not found: " (name form)))))
    (merge {:type      :atom
            :path      *path*
            :value     form
            :atom-type atom-type}
           opts)))

(defn compile
  ([expr] (compile {} expr))
  ([opts expr]
   (let [opts (merge {:prior        false
                      :mode         :write
                      :as-snapshot? false} opts)]
     (->> expr
          (expand)
          (node opts)
          (emit)))))