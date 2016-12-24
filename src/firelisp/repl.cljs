(ns firelisp.repl
  (:require [firelisp.env :refer [terminal-defs static-symbols *defs*]]
            [clojure.string :as string]))

(defn doc [sym]
  (let [{:keys [name docstring arglists siblings parent]} (or (get @terminal-defs sym)
                                                              (get @*defs* sym)
                                                              (get static-symbols sym)
                                                              (let [symbol-parts (string/split (str sym) ".")]
                                                                (cond-> (get-in static-symbols symbol-parts)
                                                                        (> (count symbol-parts) 1) (assoc :siblings (->> (get-in static-symbols (drop-last symbol-parts))
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

#_(js/setTimeout
  #(doseq [s '[+
               child
               :create
               auth.uid
               auth.token.email
               now
               get-in]] (println (doc s))) 100)