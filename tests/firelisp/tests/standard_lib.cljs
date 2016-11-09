(ns firelisp.tests.standard-lib
  (:require [devcards.core :refer-macros [deftest]]
            [firelisp.rules :as rules]
            [firelisp.compile :refer [compile-expr expand]]
            [firelisp.db :as db :include-macros true])
  (:require-macros
    [firelisp.db :refer [at throws]]
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

  (rules/rulefn admin? [uid]
                '(= (child next-root "users" ~uid "admin") true))


  (testing "Functions"
    (at "/x"
        (is (= (compile-expr {:mode :read}
                             '(admin? "mhuebert"))
               "(root.child('users' + '/' + 'mhuebert' + '/' + 'admin').val() === true)")))


    (is (= (compile-expr '(admin? auth.uid))
           "(newData.child('users' + '/' + auth.uid + '/' + 'admin').val() === true)"))
    (is (= (let [destructure-test (rules/rulefn* [x & args] '(= ~x ~(last args)))]
             (compile-expr (destructure-test "hello" "hello")))
           "('hello' === 'hello')")))

  (testing "Let"

    (is (= (compile-expr '(let [x auth.token
                           y "matt"]
                       (let [x auth.uid]
                         (and (= (get-in next-root ["admin" "uid"]) x)
                              (= (get-in next-root ["admin" "name"]) y)))))
           "((newData.child('admin' + '/' + 'uid').val() === auth.uid) && (newData.child('admin' + '/' + 'name').val() === 'matt'))")))

  "**Macros**"



  (testing "cond"
    (is (= (compile-expr '(cond 1 2
                                3 4
                                :else 5))
           "(1 ? 2 : (3 ? 4 : 5))")))

  (testing "-> (threading macro)"
    (is (= (expand '(-> next-data
                        parent
                        (child "x")
                        lower-case))
           '(lower-case (child (parent next-data) "x")))))

  (testing "every->"
    (is (= (expand '(every-> next-data string? (= "matt")))
           '(and (string? next-data) (= next-data "matt")))))

  (testing "in?"

    (let [db (-> db/blank
                 (db/rules
                   {:write true}
                   (at "users/$uid/roles"
                       {:validate '(in? #{"admin"
                                          4
                                          auth.uid} next-data)}))
                 (db/auth! {:uid "my-uid"}))]

      (is (db/set-data db "users/matt/roles" "admin")
          "in? with string")
      (is (db/set-data db "users/matt/roles" 4)
          "in? with number")
      (is (db/set-data db "users/matt/roles" "my-uid")
          "in? with auth value")
      (throws (db/set-data db "users/matt/roles" "other-string")
              "value not contained in set")
      (is (db/set-data db "users/matt/roles" nil)
          "careful - as a validate rule, nil is still allowed"))))