(ns firelisp.tests.standard-lib

  (:require [devcards.core :refer-macros [deftest]]
            [firelisp.core :as f :refer-macros [path path*]]
            [firelisp.template :refer [t] :include-macros true]
            [firelisp.compile :refer [compile-expr expand expand-1]]
            [firelisp.db :as db :include-macros true]
            [firelisp.template :refer [t] :include-macros true]
            [clojure.walk :as walk]
            [clojure.zip :as z])
  (:require-macros
    [firelisp.tests.util :refer [throws]]
    [cljs.test :refer [is are testing async]]))

(defn child-locs [loc]
  (take-while identity (iterate z/right (z/down loc))))

(defn coll-zip
  "Returns a zipper for nested sequences, given a root sequence"
  [root]
  (z/zipper coll?
            identity
            (fn [node children] (into (empty node) children))
            root))

(let [sample '(authorize {:write (let [a-str "hello"
                                       a-fn (fn [n] (+ n 1))
                                       a-fn* #(+ % 1)
                                       a-macro (macro [n a-str] (into [] (take 3 (repeat a-str))))]
                                   [a-str
                                    (a-fn 2)
                                    (a-fn* 2)
                                    (a-macro 3 "q")])})
      loc (coll-zip sample)]
  ((fn print-ops [loc]
     (if (z/branch? loc)
       (let [children (child-locs loc)]
         (when (seq? (z/node loc)) (println :op (z/node (first children))))
         (doseq [a children] (print-ops a))))) loc))

(deftest next-part
  (testing "rule-building macros"

    (is (= (path* []
                  (authorize {:write []})
                  (path* ["1"]
                         (authorize {:write ["1"]})
                         (path* ["2" two]
                                (authorize {:write ["1" "2" two]}))))
           '{:write #{[]}
             "1"    {:write #{["1"]}
                     "2"    {two {:write #{["1" "2" $two]}}}}})
        "Nested paths write rules to appropriate paths")

    (is (= (path* []
                  (authorize {:write (let [a-str "hello"
                                           a-fn (fn [n] (+ n 1))
                                           a-fn* #(+ % 1)
                                           a-macro (macro [n a-str] (into [] (take 3 (repeat a-str))))]
                                       [a-str
                                        (a-fn 2)
                                        (a-fn* 2)
                                        (a-macro 3 "q")])}))
           {:write '#{["hello", (2 + 1), (2 + 1), ["q", "q", "q"]]}}))

    ;; either we do more in the 'expand' phase, ie, resolve things,
    ;; or get rid of the 'expand' phase.
    ;;
    ;; we do not need to save this stuff for 'emit'.
    ;; bound values should be either functions, declared via f/def or f/fn,
    ;; or firelisp code.
    ;; that means they can be expanded.
    ;; `expand` should allow things to be firelisp functions.
    ;; ...and evaluate them.
    ;;
    ;; now a q, do we expand parameters beforehand, or not.
    ;; for macros the answer should be 'no'.
    ;;

    (is (= (path* [] (authorize {:write true
                                 :index #{"a" "b"}}))
           {:write #{true}
            :index #{"a" "b"}}))))

(deftest standard-lib



  (is (= (path [] (expand '(validate {"title" 'string?})))
         {:children #{"title"}
          "title"   {:validate #{'(firelisp.template/t string?)}}}))

  (is (= (path [] (expand '(authorize {:write '(get next-data "a")})))
         {:write '(child next-data "a")}))

  (testing "validate"
    (is (= 1 1)))

  #_(testing "use fn"
      (is (= (f/let [x (fn [y] (+ 1 y))]
               ;; currently: f/let has to be *outside* of the compile call.
               ;; the `path` statement does not retain or read from *context* in this way.
               ;; it only does that later, when we emit.
               ;; ...the output of `path` can still be merged with other outputs.
               ;; steps...
               ;; - write rules and link them to a rule context like :write, :read, :update
               ;; - merge all the rules
               ;; - compile
               ;;
               ;; how can we most easily share state with the Clojure runtime? let, def, etc.
               (f/compile
                 (path []
                       {:write (= auth.uid (x 2))}))
               )
             {})))

  (testing "if-let"
    (is (= (expand '(if-let [x 1] x "not-one"))
           '(let [x 1]
              (if x x "not-one"))))

    (is (= (expand '(if-let [x (= 10 10)] (+ x 1) false))
           '(let [x (= 10 10)]
              (if x (+ x 1) false)))))

  (testing "let"

    (path [xx]
          (is (= (compile-expr '(let [x auth.token]
                                  x))
                 "auth.token")))

    (is (= (compile-expr '(let [x auth.token
                                y "matt"]
                            (let [x auth.uid]
                              (and (= (get-in next-root ["admin" "uid"]) x)
                                   (= (get-in next-root ["admin" "name"]) y)))))
           "((newData.child('admin' + '/' + 'uid').val() === auth.uid) && (newData.child('admin' + '/' + 'name').val() === 'matt'))"))

    (testing "multiple bindings"

      (is (= (compile-expr '(let [x 10
                                  x 11
                                  y (+ x 12)]
                              (+ x y)))
             "(11 + (11 + 12))")
          "multiple let bindings expand to nested lets"))
    (testing "f/let"

      (is (= (f/let [a-str "hello"
                     a-fn (fn [n] (+ n 1))
                     a-fn* #(+ % 1)
                     a-macro (macro [n a-str] (into [] (take 3 (repeat a-str))))]
               (compile-expr '[a-str
                               (a-fn 2)
                               (a-fn* 2)
                               (a-macro 3 "q")]))
             "['hello', (2 + 1), (2 + 1), ['q', 'q', 'q']]")
          "f/let with string, (fn..), #(..), (macro ..)")

      (is (true? (f/let [x (fn [y] (+ 0 y))
                         x (fn [y] (+ 1 y))
                         z 99
                         z 100]

                   (and (= (compile-expr '(x 1))
                           "(1 + 1)")
                        (= (compile-expr '(x z))
                           "(1 + 100)")
                        (= (compile-expr '(let [z 101] (x z)))
                           "(1 + 101)")
                        (= (compile-expr '(let [z 101
                                                z 102] (x z)))
                           "(1 + 102)")
                        ))))))

  "**Reading from the database**

  Use `get` and `get-in` to read sub-properties of data or root.
  "
  (testing "get, get-in"

    (are [expr s]
      (= (expand expr) s)

      '(get next-data "users") '(child next-data "users")
      '(get next-data "users" "default")

      '(if (exists? (child next-data "users"))
         (child next-data "users")
         "default")
      '(get-in next-data ["address" "zip"])
      '(child next-data "address" "zip")

      '(get-in next-root ["users" auth.uid])
      '(child next-root "users" auth.uid))

    (is (= (expand '(get-in next-root ["permissions" (get-in next-root ["users" auth.uid "role"])]))
           (expand '(let [current-user-role (get-in next-root ["users" auth.uid "role"])]
                      (get-in next-root ["permissions" current-user-role])))
           '(child next-root "permissions" (child next-root "users" auth.uid "role")))
        "Nested `get-in`, also w/ `let`")

    )

  (f/defmacro admin? [uid]
    '(= (child next-root "users" ~uid "admin") true))


  (testing "Functions"
    (path ["x"]
          (is (= (compile-expr {:mode :read}
                               '(admin? "mhuebert"))
                 "(root.child('users' + '/' + 'mhuebert' + '/' + 'admin').val() === true)")))


    (is (= (compile-expr '(admin? auth.uid))
           "(newData.child('users' + '/' + auth.uid + '/' + 'admin').val() === true)"))

    ;; depracated anonymous fn/macro
    #_(is (= (let [destructure-test (f/macro* [x & args] '(= ~x ~(last args)))]
               (compile-expr (destructure-test "hello" "hello")))
             "('hello' === 'hello')")))

  "**Macros**"



  (testing "cond"
    (is (= (compile-expr '(cond 1 2
                                3 4
                                :else 5))
           "(1 ? 2 : (3 ? 4 : 5))"))

    (is (= (expand '(cond 1 2 3 4))
           '(if 1 2
                  (if 3 4))))

    (is (= (expand '(cond 1 2
                          3 4
                          :else 5))
           '(if 1 2
                  (if 3 4
                        5))))

    (is (= (expand '(cond "a" 1
                          "b" 2))
           '(if "a" 1 (if "b" 2))))

    (is (= (expand '(cond :let [x 1]
                          (= x 5) true))
           '(if (= 1 5) true)))
    (is (= (compile-expr '(cond :let [x 1]
                                (= x 5) true))
           (compile-expr '(let [x 1]
                            (cond (= x 5) true)))
           (compile-expr '(let [x 1]
                            (if (= x 5) true)))
           (compile-expr '(let [x 1]
                            (cond :when (= x 5)
                                  :else true)))
           (compile-expr '(cond :let [x 1]
                                :when-let [res (= x 5)]
                                :else true))
           (compile-expr '(cond :let [x 1
                                      res (= x 5)]
                                :when res
                                :else true))
           (compile-expr '(cond :let [x 1
                                      res (= x 5)]
                                :when res
                                true))
           "((1 === 5) ? true : false)"))

    )

  (testing "-> (threading macro)"
    (is (= (expand '(-> next-data
                        parent
                        (child "x")
                        lower-case))
           '(lower-case (child (parent next-data) "x")))))

  (testing "and->"
    (is (= (expand '(and-> next-data
                           string?
                           (= "matt")))
           '(and (string? next-data)
                 (= next-data "matt")))))

  (testing "string fns"
    (is (= (expand '(upper-case? x))
           '(= x (upper-case x)))
        "upper-case?"))

  (testing "comparisons"

    (is (= (expand '(between 0 -10 10))
           '(and (> 0 -10) (< 0 10)))
        "between")

    (is (= (expand '(within 0 -10 10))
           '(and (>= 0 -10) (<= 0 10)))
        "within"))

  (testing "includes?, set-contains?"

    (let [db (-> db/blank
                 (db/rules
                   {:write true}
                   (path ["users" uid "roles"]
                         {:validate (set-contains? #{"admin"
                                                     4
                                                     auth.uid} next-data)}))
                 (db/auth! {:uid "my-uid"}))]

      (println (:compiled-rules db))

      (is (db/set? db "users/matt/roles" "admin")
          "set-contains? with string")
      (is (db/set? db "users/matt/roles" 4)
          "set-contains? with number")
      (is (db/set? db "users/matt/roles" "my-uid")
          "set-contains? with auth value")
      (is (not (db/set? db "users/matt/roles" "other-string"))
          "value not contained in set")
      (is (db/set db "users/matt/roles" nil)
          "careful - as a validate rule, nil is still allowed")))

  (testing "namespaced symbols"

    (is (do
          (f/defn color/black? [x] (= x "#000"))
          (= (expand '(color/black? next-data.color))
             '(= next-data.color "#000")))
        "Define and use a function with a namespaced symbol"))

  (testing "def"

    (is (= (do (f/def 'xyz "ex why zee")
               (expand 'xyz)) "ex why zee")
        "Can def a value")

    (is (= (do (f/def 'colors/black "#000")
               (expand 'colors/black)) "#000"))

    )
  )