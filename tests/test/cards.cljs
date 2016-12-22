(ns test.cards
  (:require [devcards.core :as devcards :include-macros true]
            [firelisp.tests.a-simple-example]
            [firelisp.tests.db]
            [firelisp.tests.rules]
            [firelisp.tests.emit]
            [firelisp.tests.standard-lib]
            [firelisp.tests.targaryen]
            [firelisp.tests.examples]
            [firelisp.tests.next]))

(devcards/start-devcard-ui!)