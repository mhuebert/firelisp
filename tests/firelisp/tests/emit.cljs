(ns firelisp.tests.emit
  (:require
    [devcards.core :refer-macros [deftest]]
    [firelisp.db :as db :include-macros true]
    [firelisp.compile :refer [compile-expr]]
    [firelisp.next :as n :refer [expand]]
    [firelisp.core :refer [compile] :refer-macros [path] :include-macros true])
  (:require-macros
    [cljs.test :refer [is are testing]]))



(deftest built-ins

  (testing "Throw error if symbol does not exist"
    (is (thrown? js/Error (compile-expr '(nonexistent-symbol))))
    )


  (testing "String Methods"

    (are [form out]
      (= (compile-expr form) out)
      '(length next-data) "newData.val().length"
      '(length (child next-data "p")) "newData.child('p').val().length"
      '(< (length next-data) 100) "(newData.val().length < 100)"
      '(length "abc") "'abc'.length"
      '(includes? "abc" "b") "'abc'.contains('b')"
      '(includes? next-data "b") "newData.val().contains('b')"
      '(includes? "abc" next-data) "'abc'.contains(newData.val())"
      '(starts-with? "abc" next-data) "'abc'.beginsWith(newData.val())"
      '(starts-with? next-data "p") "newData.val().beginsWith('p')"
      '(ends-with? "abc" next-data) "'abc'.endsWith(newData.val())"
      '(ends-with? next-data "p") "newData.val().endsWith('p')"
      '(replace "abc" (get next-data "a") (get next-data "b")) "'abc'.replace(newData.child('a').val(), newData.child('b').val())"
      '(replace next-data "p" "q") "newData.val().replace('p', 'q')"
      '(lower-case "abc") "'abc'.toLowerCase()"
      '(lower-case next-data) "newData.val().toLowerCase()"
      '(upper-case "abc") "'abc'.toUpperCase()"
      '(upper-case next-data) "newData.val().toUpperCase()"
      '(matches? "ababa" #"bab") "'ababa'.matches(/bab/)"
      '(matches? next-data #".*") "newData.val().matches(/.*/)"))


  (testing "Logic"

    (are [form out]
      (= (compile-expr form) out)
      '(and (= 1 1) (= 2 2)) "((1 === 1) && (2 === 2))"
      '(or (exists? next-data) (exists? (get next-root "foo"))) "(newData.exists() || newData.child('foo').exists())"
      '(> next-root 0) "(newData.val() > 0)"
      '(not (= 1 2)) "!(1 === 2)"
      '(not= 1 2) "(1 !== 2)"
      '(if (= 1 1) true false) "((1 === 1) ? true : false)"))

  (testing "vector->array"
    (is (= (compile-expr '[1 2 3 4])
           "[1, 2, 3, 4]"))
    (is (= (compile-expr '[1 "hello"])
           "[1, 'hello']")))

  (testing "Snapshot methods"
    (are [expr s]
      (= (compile-expr expr) s)
      'next-data "newData.val()"
      'next-root "newData.val()"
      'prev-data "data.val()"

      '(exists? next-data) "newData.exists()"
      '(object? next-data) "newData.hasChildren()"
      '(number? next-data) "newData.isNumber()"
      '(string? next-data) "newData.isString()"
      '(string? "pete") "'pete'.isString()"
      '(boolean? next-data) "newData.isBoolean()"
      '(parent next-data) "newData.parent().val()"
      '(parent (parent next-data)) "newData.parent().parent().val()"
      '(child next-data "x") "newData.child('x').val()"
      '(child next-data "x" "y") "newData.child('x' + '/' + 'y').val()"
      '(contains-key? next-data "p") "newData.hasChild('p')"
      '(contains-keys? next-data ["p" "q"]) "newData.hasChildren(['p', 'q'])"
      '(child next-data (string? (child prev-root "x"))) "newData.child(root.child('x').isString()).val()")

    (path ["x"] (is (= (compile-expr 'next-root) "newData.parent().val()"))))

  (testing "Parent/Child Navigation (path '/$sweet' ...)"

    (path [sweet-thing]
          (are [form out]
            (= (compile-expr form) out)

            '(= (child (parent next-data) "name") "frank")
            "(newData.parent().child('name').val() === 'frank')"

            '(get-in (parent next-data) ["name" auth.uid sweet-thing :whatever])
            "newData.parent().child('name' + '/' + auth.uid + '/' + $sweet-thing + '/' + 'whatever').val()"

            '(child next-root "permissions" (child next-root "users" auth.uid "role"))
            "newData.parent().child('permissions' + '/' + newData.parent().child('users' + '/' + auth.uid + '/' + 'role').val()).val()"

            'sweet-thing "$sweet-thing"

            'auth.uid "auth.uid"
            '(= now (child prev-root "x")) "(now === root.child('x').val())"

            )))

  (testing "Previous Data"
    (are [form out]
      (= (compile-expr form) out)

      '(= (get next-data "x") true)
      "(newData.child('x').val() === true)"

      '(= (get prev-data "x") true)
      "(data.child('x').val() === true)"))

  (testing "depth"

    (is (= (compile-expr '(+ 1
                             (- 2
                                (* 3
                                   (/ 4))
                                2)))
           "(1 + (2 - (3 * 4) - 2))")))

  (testing "Root"

    (is (= (-> (path ["x"]
                     (path ["q"]
                           {:validate (= true (get next-root "q"))}))
               compile
               (get-in ["x" "q" ".validate"]))
           "(true === newData.parent().parent().child('q').val())"))

    (path ["x" y "timestamp"]

          (is (= (expand '(= next-data (get next-root "p")))
                 '(= next-data (child next-root "p")))))

    (is (= (-> db/blank
               (db/rules (path ["x" y]
                               {:validate {:timestamp (= next-data (get next-root "p"))}}))
               :compiled-rules
               (get-in ["x" "$y" "timestamp" ".validate"]))
           "(newData.val() === newData.parent().parent().parent().child('p').val())")))

  (testing "Child conversions"

    (is (= (compile-expr '(child next-root "x" "y" auth.uid))
           "newData.child('x' + '/' + 'y' + '/' + auth.uid).val()")))

  (testing "Rule expansion (path '/x' ...) "
    (path ["x"]
          (are [form expanded]
            (= (expand form) expanded)
            (and x y (and z (and p q r (or 1 2 (or 3 4)))))
            '(and x y z p q r (or 1 2 3 4))

            (= (get next-data "x") 1)
            '(= (child next-data "x") 1)

            (= next-root 1)
            '(= next-root 1)))))