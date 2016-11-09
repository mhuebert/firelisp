(ns test.cards
  (:require [devcards.core :as devcards :include-macros true]
            [firelisp.tests.a-simple-example]
            [firelisp.tests.db]
            [firelisp.tests.rules]
            [firelisp.tests.emit]
            [firelisp.tests.standard-lib]
            [firelisp.tests.targaryen]
            [firelisp.tests.examples]))

(devcards/start-devcard-ui!)