(ns test.runner
  (:require [test.cards]
            [doo.runner :refer-macros [doo-all-tests]]))

(doo-all-tests #"firelisp.*")


