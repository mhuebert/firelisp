(ns firelisp.tests.db
  (:require
    [devcards.core :refer-macros [deftest defcard]]
    [firelisp.db :as db :refer-macros [at]])
  (:require-macros
    [cljs.test :refer [is testing]]))

(let [d (-> db/blank
            (db/defn signed-in? [] '(not= auth nil))
            (db/rules
              (at "cells"
                  {:read '(signed-in?)}
                  (at "$uid"
                      {:write '(= auth.uid $uid)})))
            (db/auth! {:uid "matt"}))
      doc {:title "my-doc"}]

  (is (db/read? d "/cells"))
  (is (db/set? d "/cells/matt" doc))
  (is (false? (db/set? d "/cells/pete" doc))))

(deftest usage
  (testing "fire-db"
    (let [db (-> db/blank
                 (db/defn signed-in? [] '(not= auth nil))
                 (db/rules
                   (at "cells"
                       (at "$uid"
                           {:validate '(signed-in?)}))))]

      (is (= (-> db
                 (db/rules
                   {"/open"
                    {:read  true
                     :write true}})
                 (db/set-data "/open/1" {:name "Matt"})
                 (db/read "/open/1/name"))
             "Matt"))

      (is (thrown? js/Error
                   (-> db/blank
                       (db/rules
                         (at "/x" {:read false}))
                       (db/read "x"))))

      (let [DB (-> db/blank
                   (db/rules
                     (at "users/$uid"
                         {:read     true
                          :write    '(= $uid auth.uid)
                          :validate {:name 'string?}}))
                   (db/auth! {:uid "matt"}))]

        (is (db/read? DB "users/matt"))

        (is (= "Matt" (-> DB
                          (db/set-data "users/matt" {:name "Matt"})
                          (db/read "users/matt/name"))))

        (is (false? (db/read? DB "/"))))

      (is (= (at "/x"
                 {:read '(= auth.uid "herman")})
             (at "/x"
                 {:read '(= auth.uid "herman")})))

      (is (-> db/blank
              (db/rules
                (at "/x" {:read '(= auth.uid "herman")}))
              (db/auth! {:uid "herman"})
              (db/read "x")
              nil?))

      (is (= (:rules db)
             {"cells" {"$uid" {:validate '#{(signed-in?)}}}}))
      (is (= (:compiled-rules db)
             {"cells" {"$uid" {".validate" "(auth !== null)"}}})))))