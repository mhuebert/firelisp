(ns firelisp.tests.rules
  (:require
    [devcards.core :refer-macros [deftest]]
    [firelisp.db :as db :refer-macros [at throws]]
    [firelisp.rules :refer [compile add]]
    [firelisp.paths :refer [parse-path]])
  (:require-macros
    [cljs.test :refer [is testing async]]))


(deftest rules-at-paths

  (testing "path variables"
    (is (= (-> db/blank
               (db/rules
                 (at "$y"
                     {:read '(= auth.uid $y)}))
               :compiled-rules)
           {"$y" {".read" "(auth.uid === $y)"}})))

  (testing "paths"

    (is (= (parse-path "/") '[]))

    (is (= (parse-path "/x/y/$z/")
           '["x" "y" $z]))
    (is (= (parse-path "x/y/$z")
           '["x" "y" $z]))

    (is (= (compile (at "x/$y/$z"
                        {:read true}))
           '{"x" {"$y" {"$z" {".read" "true"}}}}))

    (is (= (compile (at "/"
                        {:read true}))
           '{".read" "true"}))

    (is (=
          (compile (at "cell"
                       {:read true}
                       (at "owner"
                           {:write '(= auth.uid next-data)})))
          {"cell" {".read" "true"
                   "owner" {".write" "(auth.uid === newData.val())"}}})))



  (testing "CRUD"

    (testing "Create"
      (let [d (atom)]

        ;; set :write rule to true
        (reset! d (-> db/blank
                      (db/rules (at "/" {:read   true
                                         :write  true
                                         :create '(= auth.uid "x")}))))

        (throws (->
                  (db/auth! @d {:uid "y"})
                  (db/set "/" "new-val"))
                "y can't create")

        (is (swap! d #(-> (db/auth! % {:uid "x"})
                          (db/set "/" "new-val")))
            "x can create")

        (is (swap! d #(-> (db/auth! % {:uid "x"})
                          (db/set "/" "other-val")))
            "x can update")

        (is (swap! d #(-> (db/auth! % {:uid "y"})
                          (db/set "/" "other-val-2")))
            "y can update")

        (is (swap! d #(-> (db/auth! % {:uid "x"})
                          (db/set "/" nil)))
            "x can delete")

        (is (= nil (db/read @d "/")))

        (throws (-> (db/auth! @d {:uid "y"})
                    (db/set "/" "new-val"))
                "y can't create - TARGAR: when val has been set to nil...")

        (reset! d (-> db/blank
                      (db/rules (at "/" {:create '(= auth.uid "x")}))))

        (is (swap! d #(-> (db/auth! % {:uid "x"})
                          (db/set "/" "new-val")))
            "can create")

        (throws (-> (db/auth! @d {:uid "x"})
                    (db/set "/" "other-val"))
                "can't update")

        (throws (-> (db/auth! @d {:uid "x"})
                    (db/set "/" nil))
                "can't delete")



        ;; set :write rule to false
        (reset! d (-> db/blank
                      (db/rules (at "/" {:write  true
                                         :create '(= auth.uid "x")}))))

        (is (swap! d #(-> (db/auth! % {:uid "x"})
                          (db/set "/" "new-val")))
            "can't create")

        ))

    (testing "Read"

      (is (= "hello" (-> db/blank
                         (db/rules (at "/" {:read '(= auth.uid "x")}))
                         (db/set! "/" "hello")
                         (db/auth! {:uid "x"})
                         (db/read "/"))))

      (throws (-> db/blank
                  (db/rules (at "/" {:read '(= auth.uid "y")}))
                  (db/set! "/" "hello")
                  (db/auth! {:uid "x"})
                  (db/read "/"))))

    (testing "Update"
      (let [d (atom (db/rules db/blank
                              (at "/" {:update '(= auth.uid "x")})))]

        ; can't create
        (throws (-> (db/auth! @d {:uid "x"})
                    (db/set "/" "new-val")))

        (swap! d db/set! "/" "old-val")

        ; can update
        (is (-> (db/auth! @d {:uid "x"})
                (db/set "/" "new-val")))

        ; can't update as y
        (throws (-> (db/auth! @d {:uid "y"})
                    (db/set "/" "new-val")))

        ; can't delete
        (throws (-> (db/auth! @d {:uid "x"})
                    (db/set "/" nil)))

        ; now let's set a general :write => true rule...
        (reset! d (-> @d
                      (db/rules (at "/" {:write true}))
                      (db/set! "/" nil)))

        ;; the :update rule is not triggered on :create, so anybody can create:
        (is (swap! d #(-> (db/auth! % nil)
                          (db/set "/" "new-val"))))
        ;; but we can't update unless we have uid of 'x':
        (throws (swap! d db/set "/" "other-val"))
        (is (-> (db/auth! @d {:uid "x"})
                (db/set "/" "other-val")))))

    (testing "Delete"
      (let [d (atom (db/rules db/blank (at "/" {:write  true
                                                :delete '(= auth.uid "x")})))]

        (is (swap! d db/set "/" "new-data")
            "create")

        (is (swap! d db/set "/" "other-data")
            "update")
        ; can't delete as y
        (throws (-> (db/auth! @d {:uid "y"})
                    (db/set "/" nil))
                "can't delete as y")

        (is (-> (db/auth! @d {:uid "x"})
                (db/set "/" nil))
            " can delete as x")

        )

      ))

  (testing "validating types"
    (let [db (-> db/blank
                 (db/rules
                   (at "/"
                       {:write true}
                       (at "auth-uid"
                           {:validate '(= next-data auth.uid)})
                       (at "number"
                           {:validate '(number? next-data)})
                       (at "string"
                           {:validate '(string? next-data)}))))]

      (is (-> (db/set! db "/" {:uid "frank"})
              (db/set "/auth-uid" "frank")))



      (is (not (db/set? db "/number" "frank")))
      (is (db/set? db "/number" 1))


      (is (not (db/set? db "/string" 1)))
      (is (db/set? db "/string" "frank"))))

  (testing "Indexes"
    (is (= (compile (at "/"
                        {:index ["title"]}))
           {".indexOn" ["title"]})))


  (testing "Rule composition"
    (at "/"
        (is (= (-> (at "/x/y" (add :delete '(= next-data auth.uid)))
                   (get-in ["x" "y" :delete]))
               '#{(= next-data auth.uid)}))))

  (testing "Priors"


    (is (= (-> (at "y/$wow"
                   {:write '(= (get next-root "x") true)})
               compile
               (get-in ["y" "$wow" ".write"]))
           "(newData.parent().parent().child('x').val() === true)"))

    (is (= (-> (at "y/z"
                   {:write '(= (get prev-root "x") true)})
               compile
               (get-in ["y" "z" ".write"]))
           "(root.child('x').val() === true)")))

  (testing "Children"
    (is (= (-> (at "/"
                   {:children ["title"]})
               compile
               (get ".validate"))
           "newData.hasChildren(['title'])")))

  (testing "Indexes"
    (is (= (-> (at "/"
                   {:index ["title"]})
               compile
               (get ".indexOn"))
           ["title"]))
    )

  (testing "Add-Rule"
    (let [rules (at "/$uid"
                    (add :delete '(= 1 2))
                    (at "/$field"
                        (add :delete '(= 1 3))))]

      (is (= (get-in rules ["$uid" :delete])
             '#{(= 1 2)}))
      (is (= (get-in rules ["$uid" "$field" :delete])
             '#{(= 1 3)}))))



  (testing "Composition"
    (-> db/blank
        (db/rules
          {:read     true
           :validate {:$child 'boolean?}}
          (at "$child"
              {:write true}))
        :rules))

  (testing "(path \"cell\" (authorize {:read true}))"
    (let [db (-> db/blank
                 (db/rules (at "cell" {:read true})))]
      (is (= (db/read? db "/") false))
      (is (= (db/read? db "/cell") true))))

  (testing "rules: {:read '(= auth.uid data.owner)}, data: {:cell {:owner 'mhuebert'}}"
    (let [db (-> db/blank
                 (db/rules (at "/" {:read '(= auth.uid (get next-data "owner"))}))
                 (db/set! "/" {:owner "mhuebert"}))]
      (is (= false (db/read? db "/")))
      (is (= true (db/read? (db/auth! db {:uid "mhuebert"}) "/")))))

  (testing "path variables"
    (let [db (-> db/blank
                 (db/rules (at "$user" {:read '(= $user auth.uid)})))]
      (is (true? (db/read? (db/auth! db {:uid "mhuebert"}) "/mhuebert")))
      (is (false? (db/read? (db/auth! db {:uid "frank"}) "/mhuebert")))))

  (testing "server value"
    (let [db (-> db/blank
                 (db/rules (at "/"
                               {:write    true
                                :validate '(= next-data now)})))]
      (is (false? (db/set? db "/" (dec (.now js/Date)))))
      (is (db/set? db "/" {".sv" "timestamp"}))))

  (testing "validations"
    (testing "Default behaviour: 'validate' makes children required, and disallows non-specified children."
      (is (= (-> db/blank
                 (db/rules (at "/"
                               {:validate '{:title string?}}))
                 :compiled-rules)
             {".validate" "newData.hasChildren(['title'])"
              "title"     {".validate" "newData.isString()"}
              "$other"    {".validate" "false"}})))

    (testing "Specify a $wild child:"

      (is (= (-> db/blank
                 (db/rules (at "/"
                               {:validate '{:title string?
                                            :$wild (not= $wild "ex")}}))
                 :compiled-rules)
             {".validate" "newData.hasChildren(['title'])"
              "title"     {".validate" "newData.isString()"}
              "$wild"     {".validate" "($wild !== 'ex')"}})))




    (testing "Use metadata to specify optional fields:"
      (is (= (-> db/blank
                 (db/rules (at "/"
                               {:validate '{:title ^:optional string?}}))
                 :compiled-rules)
             {".validate" "newData.hasChildren()"
              "title"     {".validate" "newData.isString()"}
              "$other"    {".validate" "false"}})))
    )

  )
