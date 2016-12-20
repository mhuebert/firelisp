;; documentation copied, with modifications, from https://firebase.google.com/docs/reference/security/database/

(ns firelisp.compile
  (:require [clojure.string :as string]
            [clojure.walk :as walk]
            [firelisp.env :refer [*defs* *context* terminal-defs]])
  (:require-macros [firelisp.compile :refer [defop]]))

(def munge-sym #(-> (str %)
                    (string/replace "/" "__")
                    (symbol)))
(defn wrap [s]
  (str "(" s ")"))

(defmulti emit :type)

(defn node-type [form]
  (cond (vector? form) :vector
        (seq? form) :list
        :else :atom))

(defmulti node (fn [_ form]
                 (node-type form)))

(defn infix [js-operator args]
  (if (= 1 (count args))
    (emit (first args))
    (wrap (apply str (interpose (str " " js-operator " ") (map emit args))))))

(defn method
  ([js-operator args] (method js-operator args ", "))
  ([js-operator args separator]
   (str (emit (first args)) "." js-operator (wrap (string/join separator (map emit (rest args)))))))

(defop +
  "Used to add variables or for string concatenation."
  [& numbers]
  (infix "+" numbers))

(defop -
  "Used to negate a value or subtract two values in a rules expression."
  [& numbers]
  (infix "-" numbers))

(defop *
  "Used to multiply variables in a rules expression."
  [& numbers]
  (infix "*" numbers))

(defop /
  "Used to divide variables in a rules expression."
  [& numbers]
  (infix "/" numbers))

(defop %
  "Used to find the remainder of dividing one variable by another in a rules expression."
  [num div]
  (infix "%" [num div]))

(defop >
  "Used to check if a value is greater than another value in a rules expression."
  [& expressions]
  (infix ">" expressions))

(defop <
  "Used to check if a value i s less than another value in a rules expression."
  [& expressions]
  (infix "<" expressions))

(defop >=
  "Used to check if a value is greater than or equal to another value in a rules expression."
  [& expressions]
  (infix ">=" expressions))

(defop <=
  "Used to check if a value is less than or equal to another value in a rules expression."
  [& expressions]
  (infix "<=" expressions))

(defop and
  "Evaluates to true if both operands are true. Used to evaluate multiple conditions in a rules expression."
  [& expressions]
  (infix "&&" expressions))

(defop or
  "Evaluates to true if one operand in the rules expression is true."
  [& expressions]
  (infix "||" expressions))

(defop =
  "Used to check if two variables in a rules expression have the same type and value."
  [& expressions]
  (infix "===" expressions))

(defop not=
  "Used to check if two variables in a rules expression are not equal."
  [& expressions]
  (infix "!==" expressions))

(defop not
  "Evaluates to true if its single operand is false. In rules expressions, the ! operator is often used to see if data has been written to a location."
  [expression]
  (str "!" (emit expression)))

(defop if
  "If the condition evaluates to true, the second operand is evaluated. If the condition is false, the third operand is evaluated."
  ([condition result-if-true]
    ;; recurring to `if` doesn't work, `if` is interpreted as Clojure's if
   (wrap (str (emit condition) " ? " (emit result-if-true) " : " (emit (node {} 'false)))))
  ([condition result-if-true result-if-false]
   (wrap (str (emit condition) " ? " (emit result-if-true) " : " (emit result-if-false)))))

;; snapshot methods
(defop exists?
  "Returns true if snapshot contains any data"
  [data-snapshot]
  (method "exists" [data-snapshot]))

(defop number?
  "Returns true if this RuleDataSnapshot contains a numeric value."
  [data-snapshot]
  (method "isNumber" [data-snapshot]))

(defop string?
  "Returns true if this RuleDataSnapshot contains a string value."
  [data-snapshot]
  (method "isString" [data-snapshot]))

(defop boolean?
  "Returns true if this RuleDataSnapshot contains a boolean value."
  [data-snapshot]
  (method "isBoolean" [data-snapshot]))

(defop contains-keys?
  "Returns true if snapshot is an object"
  [data-snapshot & keys]
  (method "hasChildren" (cons data-snapshot keys)))

(defop parent
  "Gets parent of snapshot"
  [data-snapshot]
  (cond-> (method "parent" [data-snapshot])
          (not (:list-as-snapshot? data-snapshot)) (str ".val()")))

(defop child
  "Gets child of snapshot. Accepts multiple keys for a nested path."
  [data-snapshot & keys]
  (cond-> (method "child" (cons data-snapshot keys) " + '/' + ")
          (not (:list-as-snapshot? data-snapshot)) (str ".val()")))

(defop contains-key?
  "Returns true if snapshot is an object containing the given key"
  [data-snapshot key]
  (method "hasChild" [data-snapshot key]))

(defop priority
  "Gets the priority of the data in the snapshot"
  [data-snapshot]
  (method "getPriority" [data-snapshot]))

;; string methods
(defop upper-case
  "Returns a copy of the string converted to upper case."
  [string]
  (method "toUpperCase" [string]))

(defop lower-case
  "Returns a copy of the string converted to lower case."
  [string]
  (method "toLowerCase" [string]))

(defop includes?
  "Returns true if the string contains the specified substring."
  [string substring]
  (method "contains" [string substring]))

(defop starts-with?
  "Returns true if the string begins with the specified substring."
  [string substring]
  (method "beginsWith" [string substring]))

(defop ends-with?
  "Returns true if the string ends with the specified substring."
  [string substring]
  (method "endsWith" [string substring]))

(defop matches?
  "Returns true if the string matches the specified regular expression literal.  The regular expression syntax is not identical to common regular expressions syntax, in particular:\n\n - * + * . ( ) [ ] { } \\ work as normal.\n - ^ and $ anchors only work if we're using them to match the first or last character in the pattern.\n - only the i (ignore case) modifier flag is supported"
  [string regexp]
  (method "matches" [string regexp]))

(defop replace
  "Returns a copy of the string with all instances of a specified substring replaced with the specified replacement string. The replace() method differs slightly from the JavaScript replace() method in that it replaces all instances of a specified substring with the specified replacement string, not just the first instance."
  [string substring replacement]
  (method "replace" [string substring replacement]))

(defop length
  "Returns the length of the string."
  [string]
  (str (emit string) ".length"))

(defop let
  "Bind symbols to values"
  [bindings body]
  (loop [pairs (partition 2 (:args bindings))
         context *context*]

    (if (empty? pairs)
      (binding [*context* context] (emit body))
      (recur (rest pairs)
             (binding [*context* context]
               (update context :bindings assoc (get (ffirst pairs) :value) (emit (second (first pairs)))))))))

(defmethod emit :vector
  [{:keys [args]}]
  (str "[" (string/join ", " (map emit args)) "]"))

(defmethod emit :list
  [{:keys [as-snapshot? operator args] :as n}]
  (apply (get-in @terminal-defs [operator :value] #(println "Operator not found: " operator firelisp.env/*context*))
         (update args 0 assoc :list-as-snapshot? as-snapshot?)))

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
          (symbol? form) :symbol
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
    :boolean (str value)
    :symbol (get-in *context* [:bindings value] (str value))
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
                                               (apply str (take (count (:path *context*))
                                                                (repeat ".parent()"))))))
                     (catch js/Error e
                       (prn (str "Invalid data reference for rule type: " mode ", " value))
                       (throw e)))
                (not as-snapshot?) (str ".val()"))
    (throw (js/Error. (str "Unrecognized atom type: " atom-type ", " value)))))



(defmethod node :vector
  [opts form]
  (merge opts
         {:type         :vector
          :args         (map (partial node (assoc opts :coerce-to-val true)) (seq form))
          :as-snapshot? false}))

(defn snapshot-method?
  "By convention, methods that operate directly on snapshots begin arglists with 'data-snapshot."
  [sym]
  (= 'data-snapshot (get-in @terminal-defs [sym :arglists 0 0])))

(defmethod node :let
  [opts [op bindings body]]
  (node (update opts :bindings merge (apply hash-map bindings)) body))

(defmethod node :list
  [opts [op & args]]
  (merge (cond-> {:type     :list
                  :operator op
                  :args     (mapv (partial node (dissoc opts :as-snapshot?)) args)}
                 (snapshot-method? op) (update-in [:args 0] assoc :as-snapshot? true))
         opts))

(defmethod node :atom
  [opts form]
  (merge {:type      :atom
          :value     form
          :atom-type (atom-type form)}
         opts))

(def ^:dynamic *mode* :write)

(defn expand-1
  ([form] (expand-1 @*defs* form))
  ([defs form]
   (walk/postwalk
     (fn [expr]
       (cond (symbol? expr)
             (or
               (get-in defs [(munge-sym expr) :value])
               (get-in *context* [:bindings (munge-sym expr)])
               expr)

             (and (seq? expr) (fn? (first expr)))
             (apply (first expr) (rest expr))

             :else expr)) form)))

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

(defn compile-expr
  ([expr] (compile-expr {} expr))
  ([opts expr]
   (let [opts (merge {:mode         *mode*
                      :as-snapshot? false} opts)]
        (->> expr
             (expand)
             (node opts)
             (emit)))))