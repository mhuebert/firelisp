(ns firelisp.tests.targaryen
  (:require
    [devcards.core :refer-macros [deftest]]
    [firelisp.db :as db :refer-macros [at throws isn't]]
    [firelisp.paths :refer [parse-path]]
    [firelisp.rules :refer [compile] :refer-macros [add with-template-quotes]]
    [firelisp.targaryen :as targar])
  (:require-macros
    [cljs.test :refer [is testing]]))

(deftest targaryen

  (testing "targaryen"

    (let [d (atom)]

      ;; set :write rule to true
      (reset! d (-> db/blank
                    (db/rules (at "/" {:read   true
                                       :write  true
                                       :create '(= auth.uid "x")}))
                    ))

      (is (swap! d #(-> (db/auth! % {:uid "x"})
                        (db/set-data "/" "new-val")))
          "x can create")

      (is (swap! d #(-> (db/auth! % {:uid "y"})
                        (db/set-data "/" "other-val")))
          "y can update")

      (is (swap! d #(-> (db/auth! % {:uid "x"})
                        (db/set-data "/" nil)))
          "x can delete")

      (is (= nil (db/read @d "/")))

      (throws (-> (db/auth! @d {:uid "y"})
                  (db/set-data "/" "new-val"))
              "y can't create"))

    (let [d (atom (-> db/blank
                      (db/rules (at "/" {:read true :write true}))
                      (db/set! "/" {})))]

      (is (nil? (db/read @d "/"))
          "Set root to empty obj > root is still null")

      (swap! d db/set! "/x" {})

      (is (nil? (db/read @d "/x"))
          "Set sub-path to empty obj > path is still null")

      (swap! d db/set! "/x" 1)

      (isn't (nil? (db/read @d "/x"))
             "Set sub-path to value > path is not null")

      (aset js/window "x" (:root @d))
      )

    (let [d (atom (-> db/blank
                      (db/rules (at "/"
                                    {:read  true #_'(exists? data)
                                     :write true}))
                      (db/set! "/" {})
                      (db/set! "/" {"x" 1})))]

      (is (= (js->clj (db/read @d "/")) {"x" 1}))
      (is (= (db/read @d "/x") 1)))

    (let [snap (.snapshot (targar/database {}) "/")]
      (is (false? (.exists snap)))
      (is (= nil (.val snap))))

    ))