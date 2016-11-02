(ns test.runner
  (:require [test.cards]
            [doo.runner])
  (:require-macros
    [doo.runner :refer [doo-all-tests]]))

(doo-all-tests #"firelisp.*")


