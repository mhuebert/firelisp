(ns firelisp.tests.a-simple-example
  (:require [devcards.core :as dc :refer-macros [deftest defcard]]
            [sablono.core :refer-macros [html]]
            [firelisp.rules :refer [compile at] :include-macros true]
            [firelisp.compile :refer [expand compile-expr]]
            [cljs.tools.reader :as r])
  (:require-macros
    [cljs.test :refer [are is testing]]))

(defn code
  ([s] (code s "clojure"))
  ([s lang]
   (dc/code-highlight (clojure.string/triml s) lang)))

(def md dc/markdown->react)

(defn mixed [& body]
  (let [parse-element (fn parse-element [el]
                        (cond (string? el) (md el)
                              (vector? el) (html el)
                              (seq? el) (map parse-element el)
                              :else el))]
    (html
      [:div
       (->> body
            (map-indexed
              #(do [:div {:key %1} (parse-element %2)])))])))

(defn row [label]
  [:tr [:td {:col-span 3
             :style    {:background "none"
                        :padding    "0px 20px"}} (md label)]])

(defn form-example-compile [& examples]
  (html
    (for [example examples
          :let [form (r/read-string example)
                compiled (compile-expr form)]]
      [:tr
       (map-indexed #(do [:td {:class-name (str "td-compare-" %1)} %2])
                    [(code (str (if (seq? form) (first form) form)))
                     (code example)
                     (code compiled "javascript")])])))


(defn form-example-expand [& examples]
  (html
    (for [example examples
          :let [form (r/read-string example)
                expanded (expand form)]]
      [:tr
       (map-indexed #(do [:td {:class-name (str "td-compare-" %1)} %2])
                    [(code (str (if (seq? form) (first form) form)))
                     (code (str example))
                     (code (str expanded))])])))

(defcard
  (html [:h1 {:style {:text-align "center"}} "FireLisp"]))

(defcard
  (mixed
    "[FireLisp](https://www.github.com/mhuebert/firelisp) is designed to help you write Firebase rules faster and more securely, from Clojure(Script). Let's begin by requiring the `firelisp.rules` namespace:"
    #_(html [:h1 {:style {:font-size  50
                          :text-align "center"}} "THIS IS ALPHA SOFTWARE"])

    (code "
  (ns my-rules.core
     (:require
       [firelisp.rules :refer [at compile] :include-macros true]))")

    "We define rules using the `rules/at` macro:"

    (code "(at \"/\" {:read true})")




    (let [example "(at \"/\"
      {:read true}
      (at \"/users/$userid\"
        {:write '(= auth.uid $userid)}))"]
      (mixed

        (md "...which can be nested:")

        (code example)

        (md "The function `rules/compile` will return a Firebase-compatible map:")

        (code
          (str (compile (at "/"
                            {:read true}
                            (at "/users/$userid"
                                {:write '(= auth.uid $userid)}))))
          "javascript")

        )

      )

    ))

(defcard Composing-Rules
  (mixed
    "Firebase supports three kinds of rules: **.read**, **.write**, and **.validate**.

  ```
    (at \"/posts/$userid\"
      {:read     true
       :write    '(= auth.uid $userid)
       :validate {:title '(and
                            (string? data)
                            (< (length data) 100))}})
  ```


  ### Write vs. Validate

  If a `write` rule succeeds at any level of the path hierarchy, none of the `write` rules below it are evaluated:

   ```
   (at \"/\"
     {:write true}
     (at \"/title\"
       {:write '(not= nil auth.uid)})) ;; this is ignored!

   ```

 ...but validation rules are always enforced for non-null values, so that we can separate the concerns of *who* is allowed to write to a path from *what* they are allowed to put there.

  ```
  (at \"/\"
    {:write true}
    (at \"/title\"
      {:validate '(string? data)})) ;; this will always be enforced, unless data is `nil`.
 ```

 Note that validation rules are ignored for paths that are set to null, so deletion rules must always be written at least one level 'up' from the relevant node.

 ### Create, Update, and Delete

  Similar to the firebase [bolt compiler](https://github.com/firebase/bolt), FireLisp compiles **create**, **update**, and **delete** rules to logically equivalent `.write` rules.


  Unlike in bolt, if a **write** rule is defined, these rules behave as **narrowing mechanisms**: the write rule **and** the create/update/delete rule must be true for the operation to succeed.

  The following rules would allow users to create and delete tweets, but never edit them:

  ```
  (at \"/tweets/$userid\"
    {:write  '(= auth.uid $userid) ;; this is enforced for all writes
     :update false})               ;; updates will always fail

     ;; :create and :delete operations will be allowed if the :write rule passes
  ```"))

(defcard Basic-Operators

  (mixed
    (md "All expressions in FireLisp must ultimately expand to the following forms, which have close Firebase Rule equivalents.")
    [:table.code-compare
     {:style {:width "100%"}}
     [:tbody
      [:tr
       [:td {:style {:padding "0 20px"}} (md "**Form**")]
       [:td {:style {:padding "0 20px"}} (md "**FireLisp Example**")]
       [:td {:style {:padding "0 20px"}} (md "**Firebase Rule Equivalent**")]]


      (row "Using `data` and `root`, you can easily read data from the current rule path, or anywhere else in the database.")

      (form-example-compile
        "data"
        "root")

      (let [expr "(at \"/users/$userid\"
  root)"
            result (atom)]
        (at "/users/$userid"
            (reset! result (compile-expr 'root)))
        [:tr
         [:td.td-compare-0 (code "root")]
         [:td (code expr)]
         [:td (dc/code-highlight @result "javascript")]])

      (row "`data` and `root` refer to the **next** state of the database, _as if the attempted write has succeeded_. This is usually best for `:validate` rules, but in `:write` rules you often want to use `prior` to read the previous state of the database:")

      (form-example-compile
        "(prior data)"
        "(prior root)")

      (row "To read child nodes, use `get` and `get-in`. As in Clojure, you can pass an additional argument to be returned if the requested key does not exist. `parent` moves one level up the database path.")

      (form-example-compile
        "(get data \"title\")"
        "(get data \"title\" \"404\")"
        "(get-in root [\"settings\" auth.uid \"email\"])"
        "(parent data)")


      (row "String methods")
      (form-example-compile
        "(upper-case \"some-string\")"
        "(lower-case \"some-string\")"
        "(contains? \"some-string\" \"om\")"
        "(starts-with? \"some-string\" \"so\")"
        "(ends-with? \"some-string\" \"so\")"
        "(matches? \"some-string\" #\"str.ng\")"
        "(replace \"some-string\" \"some\" \"my\")"
        "(length \"some-string\")")


      (row "Types")
      (form-example-compile
        "(exists? data)"
        "(number? data)"
        "(string? data)"
        "(boolean? data)"
        "(object? data)")

      (row "Logic")
      (form-example-compile
        "(and true false)"
        "(or true false)"
        "(not false)"
        "(= 1 1)"
        "(not= 1 2)"
        "(if true 1 2)")

      (row "Math")
      (form-example-compile
        "(+ 1 2)"
        "(- 1 2)"
        "(* 10 10)"
        "(/ 100 10)"
        "(% 90 10)")

      (row "Comparison")
      (form-example-compile
        "(< 1 3)"
        "(<= 1 3)"
        "(> 3 1)"
        "(>= 3 1)")


      (row "**Child**

      While `get` and `get-in` are preferred, `child` is also a valid FireLisp form:")

      (form-example-compile
        "(child data \"x\" \"y\")")

      ]])

  )

(defcard Macros

  (mixed
    "**Macros** in FireLisp are expanded at compile-time until only terminal forms remain. They make code easier to write, and you can write your own. The following macros are included by default:"
    [:table.code-compare
     {:style {:width "100%"}}
     [:tbody
      [:tr
       [:td {:style {:padding "0 20px"}} (md "**Macro**")]
       [:td {:style {:padding "0 20px"}} (md "**Example**")]
       [:td {:style {:padding "0 20px"}} (md "**Expanded Form**")]]

      (form-example-expand
        "(let [x 4]
          (+ x 1))"
        "(-> data
            parent
            (child \"title\")
            lower-case)"
        "(cond 1 2
              3 4
              :else 5)"
        "(in? #{\"a\" \"b\"} data)"

        "(every-> data
                 string?
                 (contains? \"123\"))"

        )]]

    "### Writing Macros

    In lieu of real support for functions, which is not possible in the Firebase runtime, FireLisp supports **macros** written in Clojure(Script). Macros allow us to transform syntax at compile-time, allowing for faster and easier to understand rule authoring while outputting only valid Firebase Rule expressions.

  FireLisp macros use the `template` macro from Brandon Bloom's [backtick](https://github.com/brandonbloom/backtick) to support interpolation of values into quoted Clojure code. (The symbol resolution behaviour of Clojure's syntax quote makes it unsuitable for our purposes.)

  Below, you can see how the `cond` macro rewrites its arguments into nested `if` statements. (These are further compiled to ternary operations during compilation.)"

    (code "(rulefn cond [& args]
        (loop [pairs (drop-last (partition 2 args))
               expr (last args)]
          (if-let [[pred result] (last pairs)]
            (recur (drop-last pairs)
                   '(if ~pred ~result ~expr))
            expr)))")))

(defcard Database
  (mixed
    "For ease of development and testing, FireLisp exposes a `db` namespace which you can use to write and test rules. We rely on [targaryen](https://github.com/goldibex/targaryen) for testing, **which is not yet perfect**, so please do not rely on these tests alone for production cases (yet).

    Usage:"

    (code "(let [d (-> db/blank
            (db/defn signed-in? [] '(not= auth nil))
            (db/rules
              (at \"cells\"
                  {:read '(signed-in?)}
                  (at \"$uid\"
                      {:write '(= auth.uid $uid)})))
            (db/auth! {:uid \"matt\"}))
      doc {:title \"my-doc\"}]

  (is (db/read? d \"/cells\"))
  (is (db/write? d \"/cells/matt\" doc))
  (is (false? (db/write? d \"/cells/pete\" doc))))")

    ))

