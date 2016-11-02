(ns test.runner
  (:require [static.targaryen]
            [firelisp.tests.db]
            [firelisp.tests.ruleset]
            [firelisp.tests.emit]
            [firelisp.tests.standard-lib]
            [firelisp.tests.targaryen]
            [firelisp.tests.examples]
            [doo.runner])
  (:require-macros
    [doo.runner :refer [doo-all-tests]]))

(doo-all-tests #"firelisp.*")


