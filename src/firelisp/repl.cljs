(ns firelisp.repl
  (:require [firelisp.env :refer [terminal-defs *defs*]]
            [clojure.string :as string]))

(def symbol-meta
  ;; docs for builtin symbols
  {:sources/firebase-docs "https://firebase.google.com/docs/reference/security/database/"
   "auth"                 {:anchor    "auth"
                           :docstring "A variable containing the token payload if a client is authenticated, or null if the client isn't authenticated. Firebase Realtime Database allows you to easily authenticate to several built-in providers and will generate auth tokens for them. "
                           "provider" {:docstring "The authentication method used (e.g \"password\", \"anonymous\", \"facebook\", \"github\", \"google\", or \"twitter\")."}
                           "uid"      {:docstring "A unique user id, guaranteed to be unique across all providers."}
                           "token"    {:anchor          "authtoken"
                                       :docstring       "The contents of the Firebase Auth ID token."
                                       "email"          {:docstring "The email address associated with the account, if present."}
                                       "email_verified" {:docstring "true if the user has verified they have access to the email address. Some providers automatically verify email addresses they own."}
                                       "name"           {:docstring "The user's display name, if set."}
                                       "sub"            {:docstring "The user's Firebase UID. This is unique within a project."}
                                       "firebase"       {"identities"       {:docstring "Dictionary of all the identities that are associated with this user's account. The keys of the dictionary can be any of the following: email, google.com, facebook.com, github.com, twitter.com. The values of the dictionary are arrays of unique identifiers for each identity provider associated with the account. For example, auth.token.firebase.identities[\"google.com\"][0] contains the first Google user ID associated with the account."}
                                                         "sign_in_provider" {:docstring "The sign-in provider used to obtain this token. Can be one of the following strings: custom, password, anonymous, google.com, facebook.com, github.com, twitter.com."}}
                                       "iss"            {:docstring "The issuer of the token."}
                                       "aud"            {:docstring "The audience for the token."}
                                       "auth_time"      {:docstring "The last time the user authenticated with a credential using the device receiving the token."}
                                       "iat"            {:docstring "The time at which the token was issued."}
                                       "exp"            {:docstring "The time at which the token expires."}
                                       }
                           }
   "now"                  {:anchor    "now"
                           :docstring "The now variable contains the number of milliseconds since the UNIX epoch according to the Firebase Realtime Database servers. "}
   "root"                 {:anchor    "root"
                           :docstring "The current data at the root of your Firebase Realtime Database. You can use this to read any data in your database in your rule expressions."}
   "prev-data"            {:anchor    "data"
                           :docstring "The current data in the database location of the currently executing rule (as opposed to root, which gives you the data for the root of your database)."}
   "next-data"            {:anchor    "nextdata"
                           :docstring "For .write and .validate rules, the newData variable gives you the data that will result if the write is allowed (it is a \"merging\" of the existing data plus the new data being written)."}
   :read                  {:anchor    "read"
                           :docstring "A type of Security Rule which grants a client read access to a Firebase Realtime Database location."}
   :create                {:docstring "A type of Security Rule applied only if this data snapshot is new. If a :write rule is present, acts to further restrict access, never allowing an operation denied by the :write rule."}
   :update                {:docstring "A type of Security Rule applied only if this data snapshot is being modified (does not apply to create or delete operations). If a :write rule is present, acts to further restrict access, never allowing an operation denied by the :write rule."}
   :delete                {:docstring "A type of Security Rule applied only if this data snapshot is being removed. If a :write rule is present, acts to further restrict access, never allowing an operation denied by the :write rule."}
   :write                 {:anchor    "write"
                           :docstring "A type of Security Rule which grants a client write access to a Firebase Realtime Database location. A :write rule which grants permission to write to a location will also allow writing to any descendants of that location, even if the descendants have their own :write rules which fail."}
   :validate              {:anchor    "validate"
                           :docstring "A type of Security Rule used once a :write rule has granted access, to ensure that the data being written conforms to a specific standard. In addition to a :write granting access, all relevant :validate rules must succeed before a write is allowed. A :validate rule is **not** evaluated for nodes that are being deleted."}
   :index                 {:anchor    "indexon"
                           :docstring "Tells the Firebase Realtime Database servers to index specific keys in your data to improve the performance of your queries."}
   })

(defn doc [sym]
  (let [{:keys [name docstring arglists siblings parent]} (or (get @terminal-defs sym)
                                                              (get @*defs* sym)
                                                              (get symbol-meta sym)
                                                              (let [symbol-parts (string/split (str sym) ".")]
                                                                (cond-> (get-in symbol-meta symbol-parts)
                                                                        (> (count symbol-parts) 1) (assoc :siblings (->> (get-in symbol-meta (drop-last symbol-parts))
                                                                                                                         keys
                                                                                                                         (filter string?)
                                                                                                                         sort
                                                                                                                         (mapv symbol))
                                                                                                          :parent (string/join "." (drop-last symbol-parts))))))
        name (or name sym)
        arglists (some->> arglists (string/join ", "))
        divider (str "\n" (apply str (take 10 (repeat \-))) "\n")]
    (str name
         divider
         arglists
         (when arglists divider)
         docstring
         (when siblings (str "\n\n" parent " contains: " siblings))
         "\n\n")))

(js/setTimeout
  #(doseq [s '[+
               child
               :create
               auth.uid
               auth.token.email
               now
               get-in]] (println (doc s))) 100)