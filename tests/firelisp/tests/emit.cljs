(ns firelisp.tests.emit
  (:require
    [devcards.core :refer-macros [deftest]]
    [firelisp.db :as db :refer-macros [at]]
    [firelisp.compile :refer [compile-expr expand]]
    [firelisp.rules :refer [compile] :include-macros true])
  (:require-macros
    [cljs.test :refer [is are testing]]))

(deftest built-ins

  (testing "String Methods"

    (are [form out]
      (= (compile-expr form) out)
      '(length data) "newData.val().length"
      '(length (child data "p")) "newData.child('p').val().length"
      '(< (length data) 100) "(newData.val().length < 100)"
      '(length "abc") "'abc'.length"
      '(contains? "abc" "b") "'abc'.contains('b')"
      '(contains? data "b") "newData.val().contains('b')"
      '(contains? "abc" data) "'abc'.contains(newData.val())"
      '(starts-with? "abc" data) "'abc'.beginsWith(newData.val())"
      '(starts-with? data "p") "newData.val().beginsWith('p')"
      '(ends-with? "abc" data) "'abc'.endsWith(newData.val())"
      '(ends-with? data "p") "newData.val().endsWith('p')"
      '(replace "abc" (get data "a") (get data "b")) "'abc'.replace(newData.child('a').val(), newData.child('b').val())"
      '(replace data "p" "q") "newData.val().replace('p', 'q')"
      '(lower-case "abc") "'abc'.toLowerCase()"
      '(lower-case data) "newData.val().toLowerCase()"
      '(upper-case "abc") "'abc'.toUpperCase()"
      '(upper-case data) "newData.val().toUpperCase()"
      '(matches? "ababa" #"bab") "'ababa'.matches(/bab/)"
      '(matches? data #".*") "newData.val().matches(/.*/)"))


  (testing "Logic"

    (are [form out]
      (= (compile-expr form) out)
      '(and (= 1 1) (= 2 2)) "((1 === 1) && (2 === 2))"
      '(or (exists? data) (exists? (get root "foo"))) "(newData.exists() || newData.child('foo').exists())"
      '(> root 0) "(newData.val() > 0)"
      '(not (= 1 2)) "!(1 === 2)"
      '(not= 1 2) "(1 !== 2)"
      '(if (= 1 1) true false) "((1 === 1) ? true : false)"))

  (testing "vector->array"
    (is (= (compile-expr '[1 2 3 4])
           "[1, 2, 3, 4]"))
    (is (= (compile-expr '[1 "hello"])
           "[1, 'hello']")))

  (testing "no-ops"
    (is (= (compile-expr '(prior [1]))
           (compile-expr '(do [1]))
           "[1]")))


  (testing "Snapshot methods"
    (are [expr s]
      (= (compile-expr expr) s)
      'data "newData.val()"
      'root "newData.val()"
      '(prior data) "data.val()"

      '(exists? data) "newData.exists()"
      '(object? data) "newData.hasChildren()"
      '(number? data) "newData.isNumber()"
      '(string? data) "newData.isString()"
      '(string? "pete") "'pete'.isString()"
      '(boolean? data) "newData.isBoolean()"
      '(parent data) "newData.parent().val()"
      '(parent (parent data)) "newData.parent().parent().val()"
      '(child data "x") "newData.child('x').val()"
      '(child data "x" "y") "newData.child('x' + '/' + 'y').val()"
      '(contains-key? data "p") "newData.hasChild('p')"
      '(child data (string? (child (prior root) "x"))) "newData.child(root.child('x').isString()).val()")

    (at "x" (is (= (compile-expr 'root) "newData.parent().val()"))))

  (testing "Parent/Child Navigation (path '/$sweet' ...)"
    (at "/$sweet-thing"
        (are [form out]
          (= (compile-expr form) out)

          '(= (child (parent data) "name") "frank")
          "(newData.parent().child('name').val() === 'frank')"

          '(get-in (parent data) ["name" auth.uid $sweet-thing :whatever])
          "newData.parent().child('name' + '/' + auth.uid + '/' + $sweet-thing + '/' + 'whatever').val()"

          '(child root "permissions" (child root "users" auth.uid "role"))
          "newData.parent().child('permissions' + '/' + newData.parent().child('users' + '/' + auth.uid + '/' + 'role').val()).val()"

          '$sweet-thing "$sweet-thing"

          'auth.uid "auth.uid"
          '(= now (child (prior root) "x")) "(now === root.child('x').val())"

          )))

  (testing "Priors"
    (are [form out]
      (= (compile-expr form) out)

      '(= (get data "x") true)
      "(newData.child('x').val() === true)"

      '(= (prior (get data "x")) true)
      "(data.child('x').val() === true)"))

  (testing "depth"

    (is (= (compile-expr '(+ 1
                             (- 2
                           (* 3
                              (/ 4))
                           2)))
           "(1 + (2 - (3 * 4) - 2))")))

  (testing "Root"

    (is (= (-> (at "/x"
                   (at "/q"
                       {:validate '(= true (get root "q"))}))
               compile
               (get-in ["x" "q" ".validate"]))
           "(true === newData.parent().parent().child('q').val())"))

    (at "x/$y/timestamp"

        (is (= (expand '(= data (get root "p")))
               '(= data (child root "p")))))

    (is (= (-> db/blank
               (db/rules (at "x/$y"
                             {:validate {:timestamp '(= data (get root "p"))}}))
               :compiled-rules
               (get-in ["x" "$y" "timestamp" ".validate"]))
           "(newData.val() === newData.parent().parent().parent().child('p').val())")))

  (testing "Child conversions"

    (is (= (compile-expr '(child root "x" "y" auth.uid))
           "newData.child('x' + '/' + 'y' + '/' + auth.uid).val()")))

  (testing "Rule expansion (path '/x' ...) "
    (at "x"
        (are [form expanded]
          (= (expand form) expanded)
          '(and x y (and z (and p q r (or 1 2 (or 3 4)))))
          '(and x y z p q r (or 1 2 3 4))

          '(= (get data "x") 1)
          '(= (child data "x") 1)

          '(= root 1)
          '(= root 1)))))