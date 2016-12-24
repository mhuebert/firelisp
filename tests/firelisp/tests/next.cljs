(ns firelisp.tests.next
  (:require [devcards.core :refer-macros [deftest]]
            [firelisp.core :as f :include-macros true]
            [firelisp.template :refer [t] :include-macros true]
            [firelisp.compile :as compile]
            [firelisp.next :as n :refer [path authorize validate rules expand]]
            [clojure.spec :as s :include-macros true]
            [firelisp.specs :as specs]
            [clojure.walk :as walk])
  (:require-macros
    [firelisp.tests.util :refer [throws]]
    [cljs.test :refer [is are testing async]]))

(deftest scratch

  )

(deftest firelisp-next

  (testing "rule composition"

    (is (= (path [x] (expand 'x))
           '$x)
        "Path variable expansion")

    (is (= (path [x]
             (authorize [] {:write (= "a" x)}))
           {:write '(= "a" $x)})
        "`path` introduces variables into Clojure and FireLisp scope.")


    (is (= (path [x]
             (authorize [prev next]
                        {:write (= next x)}))
           '{:write (= next-data $x)})
        "authorize bindings")

    (is (thrown? js/Error
                 (path [x]
                   (authorize [] {:write (= "a" $x)}))))

    (is (=


          (path []
            (path ["1"]
              {:write ["1"]}
              (path ["2" two]
                {:write ["1" "2" two]})))


          '{:write #{[]}
            "1"    {:write #{["1"]}
                    "2"    {two {:write #{["1" "2" $two]}}}}})
        "Return rules in vectors from `authorize` and `validate`;
        Deep-merge rules in paths;
        Traverse maps - keywords indicate rules, strings and symbols indicate paths.")

    )

  (is (= (authorize [{prev-title :title} {next-title :title}]
                    {:write (= prev-title next-title)})
         '{:write (= (child prev-data "title") (child next-data "title"))})
      "authorize destructuring")

  (is (= (path []
           (authorize [] {:write true})
           (validate [] true))
         {:write    true
          :validate true})
      "Path merges body"
      )

  (is (= (path [] (n/validate [] {"title" string?}))
         '{:validate {"title" string?}})
      "Validate")



  (testing "Destructuring in `let`"

    (is (= (expand (let [a 1] a))
           1))
    (is (= (rules [_ next-data]
                  (let [{{a :all} :acrobats} next-data]
                    a))
           '(child next-data "acrobats" "all"))))

  (testing "Function parsing"


    (is (= (expand (let [q 4
                         f (fn ([x] (+ x 2))
                             ([x y] (+ x y 3))
                             ([x y z] (+ x y z q)))]
                     [(f 1)
                      (f 1 2)
                      (f 1 2 3)]))
           '[(+ 1 2)
             (+ 1 2 3)
             (+ 1 2 3 4)])
        "Multiple-arity function")

    (is (= (expand ((fn [x] (+ 1 x)) 10)))
        "Inline function")

    (is (= (authorize [_ next-data]
                      {:write ((fn [{:keys [title]}] title) next-data)})
           '{:write (child next-data "title")}))

    (is (= (specs/destructure-arglist (s/conform :firelisp.specs/arg-list '[{[a b :as c]       :c
                                                                             {:keys [d] :as e} :e
                                                                             {g :g :as f}      :f}
                                                                            h
                                                                            [i j :as k]]))
           '([{:name a, :path [:c 0]}
              {:name b, :path [:c 1]}
              {:name c, :path [:c]}
              {:name d, :path [:e :d]}
              {:name e, :path [:e]}
              {:name g, :path [:f :g]}
              {:name f, :path [:f]}]
              [{:name h, :path []}]
              [{:name i, :path [0]} {:name j, :path [1]} {:name k, :path []}]))
        "Args lists are destructured to lists of bindings")

    )

  (testing "fns are passed evaluated arguments, macros are not.")


  #_(testing "seamless usage of local variables"
      (let [foo 222
            f (f/fn [x] (= x 111))]
        (is (= (expand (f foo))
               '(= 222 111)))))

  (testing "function/macro definition"

    (is (and (= :fn (aget (f/fn [x]) "fire$type"))
             (= :macro (aget (f/macro [x]) "fire$type")))
        "Indicate function or macro"))

  (testing "macro expansion"

    (is (= (expand '(let [x 1]
                      {"name" x}))
           {"name" 1})
        "may contain a map")

    (is (= (expand '(-> auth.x
                        (= 10)
                        true?))
           '(= true (= auth.x 10)))
        "expand macros top-down"))

  (do
    (testing "anonymous functions"

      (is (= (expand (let [f (fn [x] (+ x 1))]
                       (f 10)))
             (expand (let [f #(+ % 1)]
                       (f 10)))
             '(+ 10 1))
          "Support #(...) and (fn [] ...)")

      (is (= (expand ((fn [x] (+ x 2)) 1))
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

      (testing "Anonymous function closes over its context"


        (is (= (expand '(let [x 1
                              f (fn [a] (+ a x))
                              g #(+ % x)
                              x 2]
                          [(f x) (g x) (g (f x))]))
               '[(+ 2 1) (+ 2 1) (+ 2 1 1)])
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
      ))

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