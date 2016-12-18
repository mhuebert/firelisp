(ns firelisp.compile
  (:require [clojure.string :as string]
            [firelisp.env :refer [*defs*]]
            [firelisp.walk :refer [postorder-replace]]))

(def ^:dynamic *path* [])

(def terminal-forms '#{and or = not= + - * / % > < >= <= do not if exists? number? string? boolean? object? parent child has-key? upper-case lower-case in-string? starts-with? ends-with? matches? contains? replace length})

(def munge-sym #(-> (str %)
                    (string/replace "/" "__")
                    (symbol)))

(defn expand-1
  ([form] (expand-1 @*defs* form))
  ([fns form]
   (postorder-replace
     (fn [expr]
       (if (seq? expr)
         (if-let [operator (some->> (first expr)
                                    (munge-sym)
                                    (get fns)
                                    (:fn))]
           (apply operator (rest expr))
           (do
             (when-not (contains? terminal-forms (first expr)) (prn "Operator not found: " (first expr)))
             expr))
         expr)) form)))

(defn expand
  [expr]
  (loop [current-expr expr
         count 0]
    (let [next-expr (expand-1 @*defs* current-expr)]
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
  [{:keys [operator args as-snapshot?] :as n}]
  (let [arg-count (count args)]
    (case operator
      (+ - / * % > < >= <=) (infix (str operator) n)
      and (infix "&&" n)
      or (infix "||" n)
      = (infix "===" n)
      not= (infix "!==" n)
      do (emit (last args))
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

      object? (method "hasChildren" n)

      parent (cond-> (method "parent" n)
                     (not as-snapshot?) (str ".val()"))
      child (cond-> (method "child" n " + '/' + ")
                    (not as-snapshot?) (str ".val()"))
      has-key? (method "hasChild" n)

      ;; string methods
      upper-case (method "toUpperCase" n)
      lower-case (method "toLowerCase" n)
      in-string? (method "contains" n)
      starts-with? (method "beginsWith" n)
      ends-with? (method "endsWith" n)
      matches? (method "matches" n)
      replace (method "replace" n)
      length (str (emit (first args)) ".length"))))

(defn atom-type [form]
  (case form
    (data
      next-data
      prev-data
      root
      next-root
      prev-root) :snapshot
    nil :nil
    (= form 'now) :timestamp
    (cond (number? form) :number
          (symbol? form) (if (contains? (set *path*) form)
                           :path-variable
                           :symbol)
          (string? form) :string
          (regexp? form) :regexp
          (boolean? form) :boolean
          (keyword? form) :keyword
          :else :other)))

(defmethod emit :atom
  [{:keys [value mode as-snapshot? atom-type]}]
  (case atom-type
    :nil "null"
    :timestamp "now"
    (:number
      :regexp) value
    (:string
      :keyword) (str "'" (name value) "'")
    (:boolean
      :symbol) (str value)
    :path-variable (str "$" value)
    :snapshot (cond->
                (try (case mode
                       :read (case value
                               (data
                                 next-data
                                 prev-data) "data"
                               (root
                                 next-root
                                 prev-root) "root")
                       :write (case value
                                prev-data "data"
                                next-data "newData"
                                prev-root "root"
                                next-root (str "newData"
                                               (apply str (take (count *path*)
                                                                (repeat ".parent()"))))))
                     (catch js/Error e
                       (prn (str "Invalid data reference for rule type: " mode ", " value))
                       (throw e)))
                (not as-snapshot?) (str ".val()"))
    (throw (js/Error. (str "Unrecognized atom type: " atom-type ", " value)))))

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
  (contains? #{'exists?
               'number?
               'string?
               'boolean?
               'object?
               'parent
               'child
               'has-key?} sym))

(defn no-op? [form]
  (and (seq? form)
       (= 'do (first form))))

(defmethod node :list
  [opts form]
  (let [no-op? (no-op? form)
        first-arg-as-snapshot? (or (snapshot-method? (first form))
                                   (and (:as-snapshot? opts) no-op?))
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
  (merge {:type      :atom
          :path      *path*
          :value     form
          :atom-type (atom-type form)}
         opts))

(def ^:dynamic *mode* :write)
(defn compile-expr
  ([expr] (compile-expr {} expr))
  ([opts expr]
   (let [opts (merge {:mode         *mode*
                      :as-snapshot? false} opts)]
     (->> expr
          (expand)
          (node opts)
          (emit)))))