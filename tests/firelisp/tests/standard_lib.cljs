(ns firelisp.tests.standard-lib
  (:require [devcards.core :refer-macros [deftest]]
            [firelisp.core :as f :refer-macros [at]]
            [firelisp.compile :refer [compile-expr expand expand-1]]
            [firelisp.db :as db :include-macros true])
  (:require-macros
    [firelisp.tests.util :refer [throws]]
    [cljs.test :refer [is are testing async]]))


(deftest standard-lib

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
    (at ["x"]
        (is (= (compile-expr {:mode :read}
                             '(admin? "mhuebert"))
               "(root.child('users' + '/' + 'mhuebert' + '/' + 'admin').val() === true)")))


    (is (= (compile-expr '(admin? auth.uid))
           "(newData.child('users' + '/' + auth.uid + '/' + 'admin').val() === true)"))

    ;; depracated anonymous fn/macro
    #_(is (= (let [destructure-test (f/macro [x & args] '(= ~x ~(last args)))]
               (compile-expr (destructure-test "hello" "hello")))
             "('hello' === 'hello')")))

  (testing "let"

    (is (= (compile-expr '(let [x auth.token
                                y "matt"]
                            (let [x auth.uid]
                              (and (= (get-in next-root ["admin" "uid"]) x)
                                   (= (get-in next-root ["admin" "name"]) y)))))
           "((newData.child('admin' + '/' + 'uid').val() === auth.uid) && (newData.child('admin' + '/' + 'name').val() === 'matt'))"))

    (testing "multiple bindings"
      (is (= (expand-1 '(let [x 10
                              x 11
                              y (+ x 12)]
                          (+ x y)))
             '(let [x 10]
                (let [x 11]
                  (let [y (+ x 12)]
                    (+ x y)))))
          "multiple let bindings expand to nested lets")
      (is (= (expand '(let [x 10
                            x 11
                            y (+ x 12)]
                        (+ x y)))
             '(+ 11 11 12)))))

  "**Macros**"



  (testing "cond"
    (is (= (compile-expr '(cond 1 2
                                3 4
                                :else 5))
           "(1 ? 2 : (3 ? 4 : 5))"))

    (is (= (expand '(cond 1 2
                          3 4
                          :else 5))
           '(if 1 2
                  (if 3 4
                        5))))

    (is (= (expand '(cond "a" 1
                          "b" 2))
           (expand '(cond "a" 1
                          "b" 2
                          :else false))
           '(if "a" 1 (if "b" 2 false)))
        "if no :else predicate, default to false"))

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

  (testing "includes?, in-set?"

    (let [db (-> db/blank
                 (db/rules
                   {:write true}
                   (at ["users" uid "roles"]
                       {:validate (in-set? #{"admin"
                                             4
                                             auth.uid} next-data)}))
                 (db/auth! {:uid "my-uid"}))]

      (is (db/set? db "users/matt/roles" "admin")
          "in-set? with string")
      (is (db/set? db "users/matt/roles" 4)
          "in-set? with number")
      (is (db/set? db "users/matt/roles" "my-uid")
          "in-set? with auth value")
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