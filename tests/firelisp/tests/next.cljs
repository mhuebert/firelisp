(ns firelisp.tests.next
  (:require [devcards.core :refer-macros [deftest]]
            [firelisp.core :as f :include-macros true]
            [firelisp.template :refer [t] :include-macros true]
            [firelisp.compile :as compile]
            [firelisp.next :as n :refer-macros [path authorize validate expand def-anonymous-fns]]

            )
  (:require-macros
    [firelisp.tests.util :refer [throws]]
    [cljs.test :refer [is are testing async]]))



(deftest firelisp-next

  (testing ""
    (is (= (expand (let [f (fn [x] (+ x 1))]
                         (f 10)))
           '(+ 10 1))))

  (testing "expand"
    (is (= (expand (let [x 1] x))
           1))

    (is (= (expand (let [x 1 y 2] [x y]))
           [1 2]))

    (is (= (expand (let [x 1 x 2] x))
           2))

    (is (= (expand (let [x 1 x 2 y (+ x 3)] y))
           '(+ 2 3)))

    (is (= (f/let [x 1] (expand x))
           1))

    (is (= (expand (let [f (fn [x] (* x 10))] (f 1)))
           '(* 1 10)))

    )

  #_(testing "rules"

      (is (= (authorize [prev next]
                        {:write '(let [y 2] 2)})))

      (is (=
            (authorize [prev next]
                       (let [y 2]
                         {:write 'y}))
            {:write [2]})
          "Returns a map of expanded rules")



      (is (=
            (f/let [x 1]
              (authorize [] {:write 'true}))
            {:write [{:rule    'true
                      :context {:bindings '{x 1}}}]})
          "Captures local firelisp context (from f/let)")

      (is (fn? (-> (f/let [y (f/fn [z] (+ z 1))]
                     (authorize [] {:write 'true}))
                   (get-in [:write 0 :context :bindings 'y])))
          "Capture function in binding")

      (is (= (authorize []
                        (assoc {}
                          :write 'true
                          :read 'false))
             {:write [{:rule    'true
                       :context {:bindings {}}}]
              :read  [{:rule    'false
                       :context {:bindings {}}}]})
          "Manipulate the rule map using Clojure functions")

      (is (=
            (authorize []
                       (let [x 1]
                         {:write 'true}))
            {:write [{:rule    'true
                      :context {:bindings '{x 1}}}]})
          "Captures interior context")

      )

  #_(testing "validate"
      (is (= (validate [data] 'true)
             (authorize [data] {:validate 'true}))))

  #_(testing "path"

      (is (=
            (path []
              (let [x 1]
                (authorize [] {:write 'true})))
            {:write [{:rule    'true
                      :context {:bindings '{x 1}}}]})
          "f/let is used inside `path` macro")))