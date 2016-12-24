(ns firelisp.tests.next
  (:require [devcards.core :refer-macros [deftest]]
            [firelisp.core :as f :include-macros true]
            [firelisp.template :refer [t] :include-macros true]
            [firelisp.compile :as compile]
            [firelisp.next :as n :refer [path authorize validate expand]]
            [clojure.spec :as s :include-macros true]
            [firelisp.specs :as specs]
            [clojure.walk :as walk])
  (:require-macros
    [firelisp.tests.util :refer [throws]]
    [cljs.test :refer [is are testing async]]))

(deftest firelisp-next

  (testing "seamless usage of local variables"
    (let [foo 222
          f (f/fn [x] (= x 111))]
      (is (= (expand (f foo))
             '(= 222 111)))))

  (testing "macro expansion"

    (is (= (expand '(let [x 1]
                      {"name" x}))
           {"name" 1})
        "may contain a map")

    (is (= (expand '(-> y
                        (= 10)
                        true?))
           '(= true (= y 10)))
        "expand macros top-down"))

  (testing "anonymous functions"

    (is (= (expand (let [f (fn [x] (+ x 1))]
                     (f 10)))
           (expand (let [f #(+ % 1)]
                     (f 10)))
           '(+ 10 1))
        "Support #(...) and (fn [] ...)")

    (is (= (expand ((macro [x] '(+ ~x 2)) 1))
           (expand ((fn [x] (+ x 2)) 1))
           '(+ 1 2))
        "Inline functions")

    (is (= (expand (let [x 10
                         f (fn [y] (+ y x))]
                     (f 1)))
           '(+ 1 10))
        "Functions can use variables in scope")

    (is (= (f/let [x 0
                   f (fn [a] (+ a x))]
             (expand (let [x 1
                           g (fn [b] (f b))
                           h (fn [n] (- (g n)))]
                       [(g x)
                        (f x)
                        (h x)])))
           '[(+ 1 0)
             (+ 1 0)
             (- (+ 1 0))])
        "Functions can call each other")

    (is (= (f/let [macro-a (macro [n a-str] (into [] (take n (repeat a-str))))]
             (expand '(let [macro-b (macro [n a-str] (into [] (take n (repeat a-str))))]
                        [(macro-a 1 "a")
                         (macro-b 2 "b")
                         ])))
           '[["a"] ["b" "b"]])
        "Anonymous macros")

    (testing "Anonymous function closes over its context"


      (is (= (expand '(let [x 1
                            f (fn [a] (+ a x))
                            g #(+ % x)
                            x 2]
                        [(f "a") (g "a")]))
             '[(+ "a" 1) (+ "a" 1)])
          "`let` inside expand")

      (is (= (f/let [x 1
                     f (fn [a] (+ a x))]
               (expand '(let [g #(+ % x)
                              x 2]
                          [(f "a") (g "a")])))
             '[(+ "a" 1) (+ "a" 1)])
          "`let` outside expand")))


  (testing "expand"
    (is (= (expand (let [x 1] x))
           1))

    (is (= (expand (let [x 1 y 2]
                     [x y]))
           [1 2]))

    (is (= (expand (let [x 1 x 2]
                     x))
           2))

    (is (= (expand (let [x 1
                         x 2
                         y (+ x 3)]
                     y))
           '(+ 2 3)))

    (is (= (f/let [x 1] (expand x))
           1))

    (is (= (expand (let [f (fn [x] (* x 10))]
                     (f 1)))
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