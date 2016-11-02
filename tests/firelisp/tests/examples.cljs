(ns firelisp.tests.examples
  (:require
    [devcards.core :refer-macros [deftest]]
    [firelisp.common :refer [append]]
    [firelisp.db :as db :refer-macros [at rules]]
    [firelisp.rules :refer [compile]]
    [firelisp.targaryen :as targar :refer [read? write? update? try-write]])
  (:require-macros
    [cljs.test :refer [is testing]]))

(defn log [s]
  (cljs.pprint/pprint s))

(deftest examples
  (testing "auth"

    (let [db (atom (-> db/blank

                       (db/defn role [uid]
                                "Get role for user"
                                '(prior (get-in root ["users" ~uid "role"])))

                       (db/defn has-permission? [uid action]
                                '(prior (= true (get-in root ["roles" (role ~uid) "permissions" ~action]))))

                       (db/rules

                         (at "users/$uid/role"
                             {:write '(or (and (nil? data) (= $uid auth.uid))
                                          (has-permission? auth.uid "manage-roles"))})

                         (at "roles/$role/permissions/$permission"
                             {:validate '(boolean? data)}))

                       (db/set! "/" {"roles" {"admin"  {:permissions {:read         true
                                                                      :write        true
                                                                      :manage-roles true}}
                                              "editor" {:permissions {:read  true
                                                                      :write true}}
                                              "member" {:permissions {:read true}}}
                                     "users" {:el-hefe {:role "admin"}
                                              :franco  {:role "member"}}})))]


      (is (thrown? js/Error (-> (db/auth! @db {:uid "franco"})
                                (db/set-data "users/franco/role" "admin")))
          "Member cannot set own role")

      (is (-> (db/auth! @db {:uid "el-hefe"})
              (db/set-data "users/franco/role" "admin"))
          "Admin can set roles")
      (do (-> (db/auth! @db {:uid "franco"})
              (db/set-data "users/franco/role" nil))
          "Member can delete own role")
      (is (thrown? js/Error (-> (db/auth! @db {:uid "my-uid"})
                                (db/set-data "users/my-uid/roles/x" true)))
          "Role must exist")
      (is (thrown? js/Error (-> (db/auth! @db {:uid "my-uid"})
                                (db/set-data "users/my-uid/roles/editor" "true")))
          "Role must be boolean")

      ))

  (testing "Read-example"

    (let [db (-> db/blank
                 (db/rules (at "/orders"
                               {:read '(or (not= nil (get-in root ["technicians" auth.uid]))
                                           (= "server" auth.uid))})))]

      (is (false? (db/read? db "/orders/")))

      (is (true? (->
                   (db/auth! db {:uid "server"})
                   (db/read? "/orders/"))))

      (is (true? (-> db
                     (db/set! "/technicians/123" true)
                     (db/auth! {:uid "123"})
                     (db/read? "/orders/"))))))


  (testing "Authenticated Chat"

    (let [db (atom (-> db/blank

                       (db/defn signed-in? []
                                '(not= auth nil))

                       (db/defn name-string? []
                                '(and (string? data)
                                    (between (length data) 0 20)))

                       (db/defn room-name [id]
                                '(prior (get-in root ["room_names" ~id])))

                       (db/defn room-member? [room-id]
                                '(and (signed-in?)
                                    (exists? (prior (get-in root ["members" ~room-id auth.uid])))))
                       (db/rules (at "/"
                                     {:read true}

                                     (at "room_names"
                                         {:validate 'object?}
                                         (at "$name"
                                             {:validate 'string?}))

                                     (at "members/$roomId"
                                         {:read '(room-member? $roomId)}
                                         (at "$user_id"
                                             {:validate 'name-string?
                                              :write    '(= auth.uid $user_id)
                                              :update   '(unchanged? data)}))

                                     (at "messages/$roomId"
                                         {:read     '(room-member? $roomId)
                                          :validate '(exists? (room-name $roomId))}
                                         (at "$message-id"
                                             {:write    '(= (get-in root ["members" $roomId auth.uid])
                                                            (get data "name"))
                                              :update   false
                                              :delete   false
                                              :validate {"name"      'name-string?
                                                         "message"   '(and (string? data)
                                                                           (between (length data) 0 50))
                                                         "timestamp" '(= data now)}}))))
                       (db/auth! {:uid "bob"})))]

      (is (swap! db db/set! "/room_names/y" "my-great-room"))

      (is (swap! db db/set-data "members/y/bob" "Bob"))
      (is (swap! db db/set-data "messages/y/1" {:name      "Bob"
                                                     :message   "My message"
                                                     :timestamp {".sv" "timestamp"}}))

      ))

  (testing "Throttle"

    ; "From [this example by katowulf,](http://jsfiddle.net/firebase/VBmA5/) 'throttle messages to no more than one every 5,000 milliseconds'"

    (let [rules (compile (at "/"
                             {"last_message/$user"
                                  {:write  '(= $user auth.uid)
                                   :create '(= data now)
                                   :update '(and (= data now)
                                                 (> data (+ 5000 (prior data))))
                                   :delete false}

                                  "messages/$message-id"
                                  {:write    '(= (get data "sender") auth.uid)
                                   :validate {:timestamp '(= data (get-in root ["last_message" auth.uid]))
                                              :sender    true
                                              :message   '(and (string? data)
                                                               (< (length data) 500))}}
                                  "what/$ever"
                                  {:write '(= data (child root auth.uid auth.uid))}}))
          auth {:uid "frank"}]

      (is (do "Write timestamp to blank db"
              (write? {} rules auth "last_message/frank" {".sv" "timestamp"})))
      (is (do "false: Write another timestamp less than 5000 ms after the previous"
              (false? (write? {:last_message {:frank (- (.now js/Date) 4700)}} rules auth "last_message/frank" {".sv" "timestamp"}))))
      (is (do "Write another timestamp more than 5000 ms after the previous"
              (write? {:last_message {:frank (- (.now js/Date) 5300)}} rules auth "last_message/frank" {".sv" "timestamp"})))
      (is (do "false: Delete a timestamp"
              (false? (write? {:last_message {:frank (.now js/Date)}} rules auth "last_message/frank" nil))))

      (is (do "Write message"
              (update? {} rules auth "/" {"last_message/frank" {".sv" "timestamp"}
                                          "messages/1"         {:timestamp {".sv" "timestamp"}
                                                                :sender    "frank"
                                                                :message   "Hello, there!"}})))


      (is (do "Write second message long into the future"
              (update? {:last_message {:frank 100}
                        :messages     {"1" {:timestamp 100
                                            :sender    "frank"
                                            :message   "Hello, there!"}}}
                       rules auth "/" {"last_message/frank" {".sv" "timestamp"}
                                       "messages/2"         {:timestamp {".sv" "timestamp"}
                                                             :sender    "frank"
                                                             :message   "Hello?"}})))

      (is (do "Write second message almost immediately"
              (false? (update? {:last_message {:frank (- (.now js/Date) 100)}
                                :messages     {"1" {:timestamp (- (.now js/Date) 100)
                                                    :sender    "frank"
                                                    :message   "Hello, there!"}}}
                               rules auth "/" {"last_message/frank" {".sv" "timestamp"}
                                               "messages/2"         {:timestamp {".sv" "timestamp"}
                                                                     :sender    "frank"
                                                                     :message   "Hello?"}}))))))

  (testing "Timestamps"
    (let [Post {:message  'string?
                :modified '(= data now)
                :created  '(= data (if (new? data)
                                     now
                                     (prior data)))}
          DB (-> db/blank
                 (db/rules (at "posts/$id"
                               {:read     true
                                :write    true
                                :validate Post})))]

      (is (db/set? DB "/posts/123" {:message  ""
                                       :modified {:.sv "timestamp"}
                                       :created  {:.sv "timestamp"}}))

      (is (false? (db/set? DB "/posts/123" {:message  ""
                                               :modified (dec (.now js/Date))
                                               :created  (dec (.now js/Date))})))


      (let [DB (db/set! DB "/" {:posts {"123" {:message  ""
                                               :created  (dec (.now js/Date))
                                               :modified (dec (.now js/Date))}}})]

        (is (db/set-data DB "/posts/123/modified" {:.sv "timestamp"}))

        (doseq [not-now [(dec (.now js/Date)) (+ 10 (.now js/Date))]]
          (is (false? (db/set? DB  "/posts/123/modified" not-now))))

        (is (false? (db/set? DB "/posts/123/created" {:.sv "timestamp"})))))))