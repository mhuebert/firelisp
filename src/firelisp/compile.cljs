;; documentation copied, with modifications, from https://firebase.google.com/docs/reference/security/database/

(ns firelisp.compile
  (:require [clojure.string :as string]
            [firelisp.env :refer [*defs*]]
            [clojure.set :as set]
            [firelisp.walk :refer [postorder-replace]])
  (:require-macros [firelisp.compile :refer [defop]]))

(def ^:dynamic *path* [])

(def munge-sym #(-> (str %)
                    (string/replace "/" "__")
                    (symbol)))



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

(def root-url "https://firebase.google.com/docs/reference/security/database/")
(def env-meta
  {"auth"      {:anchor    "auth"
                :docstring "A variable containing the token payload if a client is authenticated, or null if the client isn't authenticated. Firebase Realtime Database allows you to easily authenticate to several built-in providers and will generate auth tokens for them. "
                "provider" {:docstring "The authentication method used (e.g \"password\", \"anonymous\", \"facebook\", \"github\", \"google\", or \"twitter\")."}
                "uid"      {:docstring "A unique user id, guaranteed to be unique across all providers."}
                "token"    {:anchor          "authtoken"
                            :docstring       "The contents of the Firebase Auth ID token."
                            "email"          {:docstring "The email address associated with the account, if present."}
                            "email_verified" {:docstring "true if the user has verified they have access to the email address. Some providers automatically verify email addresses they own."}
                            "name"           {:docstring "The user's display name, if set."}
                            "sub"            {:docstring "The user's Firebase UID. This is unique within a project."}
                            "firebase"       {"identities"       {:docstring "Dictionary of all the identities that are associated with this user's account. The keys of the dictionary can be any of the following: email, google.com, facebook.com, github.com, twitter.com. The values of the dictionary are arrays of unique identifiers for each identity provider associated with the account. For example, auth.token.firebase.identities[\"google.com\"][0] contains the first Google user ID associated with the account."}
                                              "sign_in_provider" {:docstring "The sign-in provider used to obtain this token. Can be one of the following strings: custom, password, anonymous, google.com, facebook.com, github.com, twitter.com."}}
                            "iss"            {:docstring "The issuer of the token."}
                            "aud"            {:docstring "The audience for the token."}
                            "auth_time"      {:docstring "The last time the user authenticated with a credential using the device receiving the token."}
                            "iat"            {:docstring "The time at which the token was issued."}
                            "exp"            {:docstring "The time at which the token expires."}
                            }
                }
   "now"       {:anchor    "now"
                :docstring "The now variable contains the number of milliseconds since the UNIX epoch according to the Firebase Realtime Database servers. "}
   "root"      {:anchor    "root"
                :docstring "The current data at the root of your Firebase Realtime Database. You can use this to read any data in your database in your rule expressions."}

   "prev-data" {:anchor    "data"
                :docstring "The current data in the database location of the currently executing rule (as opposed to root, which gives you the data for the root of your database)."}
   "next-data" {:anchor    "nextdata"
                :docstring "For .write and .validate rules, the newData variable gives you the data that will result if the write is allowed (it is a \"merging\" of the existing data plus the new data being written)."}
   :read       {:anchor    "read"
                :docstring "A type of Security Rule which grants a client read access to a Firebase Realtime Database location."}
   :write      {:anchor    "write"
                :docstring "A type of Security Rule which grants a client write access to a Firebase Realtime Database location. A :write rule which grants permission to write to a location will also allow writing to any descendants of that location, even if the descendants have their own :write rules which fail."}
   :validate   {:anchor    "validate"
                :docstring "A :validate rule is used once a :write rule has granted access, to ensure that the data being written conforms to a specific standard. In addition to a :write granting access, all relevant :validate rules must succeed before a write is allowed."}
   :index      {:anchor    "indexon"
                :docstring "Tells the Firebase Realtime Database servers to index specific keys in your data to improve the performance of your queries."}
   })

(defonce operators* (atom {}))

(def operators
  {'+    {:fn        (partial infix "+")
          :docstring "Used to add variables or for string concatenation."
          :arglists  '[[& numbers]]}
   '-    {:fn        (partial infix "-")
          :docstring "Used to negate a value or subtract two values in a rules expression."
          :arglists  '[& numbers]}
   '*    {:fn        (partial infix "*")
          :docstring "Used to multiply variables in a rules expression."
          :arglists  '[[& numbers]]}
   '/    {:fn        (partial infix "/")
          :docstring "Used to divide variables in a rules expression."
          :arglists  '[[& numbers]]}
   '%    {:fn        (partial infix "%")
          :docstring "Used to find the remainder of dividing one variable by another in a rules expression."
          :arglists  '[[number number]]}
   '>    {:fn        (partial infix ">")
          :docstring "Used to check if a value is greater than another value in a rules expression."
          :arglists  '[[& expressions]]}
   '<    {:fn        (partial infix "<")
          :docstring "Used to check if a value i s less than another value in a rules expression."
          :arglists  '[[& expressions]]}
   '>=   {:fn        (partial infix ">=")
          :docstring "Used to check if a value is greater than or equal to another value in a rules expression."
          :arglists  '[[& expressions]]}
   '<=   {:fn        (partial infix "<=")
          :docstring "Used to check if a value is less than or equal to another value in a rules expression."
          :arglists  '[[& expressions]]}
   'and  {:fn        (partial infix "&&")
          :docstring "Evaluates to true if both operands are true. Used to evaluate multiple conditions in a rules expression."
          :arglists  '[[& expressions]]}
   'or   {:fn        (partial infix "||")
          :docstring "Evaluates to true if one operand in the rules expression is true."
          :arglists  '[[& expressions]]}
   '=    {:fn        (partial infix "===")
          :docstring "Used to check if two variables in a rules expression have the same type and value."
          :arglists  '[[& expressions]]}
   'not= {:fn        (partial infix "!==")
          :docstring "Used to check if two variables in a rules expression are not equal."
          :arglists  '[[& expressions]]}
   'not  {:fn        #(do (assert (= 1 (count (:args %))))
                          (str "!" (-> % :args last emit)))
          :docstring "Evaluates to true if its single operand is false. In rules expressions, the ! operator is often used to see if data has been written to a location."
          :arglists  '[[expression]]}
   'if   {:fn        (fn [{:keys [args]}]
                       (assert (#{2 3} (count args)))
                       (let [[pred e1 e2] (map (partial emit) args)]
                         (wrap (str pred " ? " e1 " : " e2))))
          :docstring "If the condition evaluates to true, the second operand is evaluated. If the condition is false, the third operand is evaluated."
          :arglists  '[[condition result-if-true] [condition result-if-true result-if-false]]}

   ;; snapshot methods
         'exists? {:fn        (partial method "exists")
                   :docstring "Returns true if snapshot contains any data"
                   :arglists  '[[data-snapshot]]}
         'number? {:fn        (partial method "isNumber")
                   :docstring "Returns true if this RuleDataSnapshot contains a numeric value."
                   :arglists  '[[data-snapshot]]}
         'string? {:fn        (partial method "isString")
                   :docstring "Returns true if this RuleDataSnapshot contains a string value."
                   :arglists  '[[data-snapshot]]}
         'boolean? {:fn        (partial method "isBoolean")
                    :docstring "Returns true if this RuleDataSnapshot contains a boolean value."
                    :arglists  '[[data-snapshot]]}
         'contains-keys? {:fn        (partial method "hasChildren")
                          :docstring "Returns true if snapshot is an object"
                          :arglists  '[[data-snapshot]]}

         'parent {:fn        (fn [{:keys [as-snapshot?] :as n}]
                               (cond-> (method "parent" n)
                                       (not as-snapshot?) (str ".val()")))
                  :docstring "Gets parent of snapshot"
                  :arglists  '[[data-snapshot]]}
         'child {:fn        (fn [{:keys [as-snapshot?] :as n}]
                              (cond-> (method "child" n " + '/' + ")
                                      (not as-snapshot?) (str ".val()")))
                 :docstring "Gets child of snapshot. Accepts multiple keys for a nested path."
                 :arglists  '[[data-snapshot & keys]]}
         'contains-key? {:fn        (partial method "hasChild")
                         :docstring "Returns true if snapshot is an object containing the given key"
                         :arglists  '[[data-snapshot key]]}

         'priority {:fn        (partial method "getPriority")
                    :docstring "Gets the priority of the data in the snapshot"
                    :arglists  '[[data-snapshot]]}

   ;; string methods
         'upper-case {:fn        (partial method "toUpperCase")
                      :docstring "Returns a copy of the string converted to upper case."
                      :arglists  '[[string]]}
         'lower-case {:fn        (partial method "toLowerCase")
                      :docstring "Returns a copy of the string converted to lower case."
                      :arglists  '[[string]]}
         'includes? {:fn        (partial method "contains")
                     :docstring "Returns true if the string contains the specified substring."
                     :arglists  '[[string substring]]}
         'starts-with? {:fn        (partial method "beginsWith")
                        :docstring "Returns true if the string begins with the specified substring."
                        :arglists  '[[string substring]]}
         'ends-with? {:fn        (partial method "endsWith")
                      :docstring "Returns true if the string ends with the specified substring."
                      :arglists  '[[string substring]]}
         'matches? {:fn        (partial method "matches")
                    :docstring "Returns true if the string matches the specified regular expression literal.  The regular expression syntax is not identical to common regular expressions syntax, in particular:\n\n - * + * . ( ) [ ] { } \\ work as normal.\n - ^ and $ anchors only work if we're using them to match the first or last character in the pattern.\n - only the i (ignore case) modifier flag is supported"
                    :arglists  '[[string regexp]]}
         'replace {:fn        (partial method "replace")
                   :docstring "Returns a copy of the string with all instances of a specified substring replaced with the specified replacement string. The replace() method differs slightly from the JavaScript replace() method in that it replaces all instances of a specified substring with the specified replacement string, not just the first instance."
                   :arglists  '[[string substring replacement]]}
         'length {:fn        (fn [{:keys [args]}] (str (emit (first args)) ".length"))
                  :docstring "Returns the length of the string."
                  :arglists  '[[string]]}

   })

(defop if
       "If the condition evaluates to true, the second operand is evaluated. If the condition is false, the third operand is evaluated."
       ([condition result-if-true]
         (if condition result-if-true false))
       ([condition result-if-true result-if-false]
         (wrap (str (emit condition) " ? " (emit result-if-true) " : " (emit result-if-false)))))

(defop not [expr]
       "not")

(defmethod emit :list
  [{:keys [operator] :as n}]
  ((get-in operators [operator :fn] #(println "Operator not found: " operator)) n))

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
  (= 'data-snapshot (get-in operators [sym :arglists 0 0])))

(defmethod node :list
  [opts form]
  (let [first-arg-as-snapshot? (snapshot-method? (first form))
        args (cons (node (cond-> opts
                                 first-arg-as-snapshot? (assoc :as-snapshot? true)) (first (rest form)))
                   (map (partial node opts) (drop 2 form)))]
    (merge {:type     :list
            :path     *path*
            :operator (first form)
            :args     args}
           opts)))

(defmethod node :atom
  [opts form]
  (merge {:type      :atom
          :path      *path*
          :value     form
          :atom-type (atom-type form)}
         opts))

(def ^:dynamic *mode* :write)

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
             (when-not (contains? operators (first expr)) (prn "Operator not found: " (first expr)))
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

(defn compile-expr
  ([expr] (compile-expr {} expr))
  ([opts expr]
   (let [opts (merge {:mode         *mode*
                      :as-snapshot? false} opts)]
     (->> expr
          (expand)
          (node opts)
          (emit)))))