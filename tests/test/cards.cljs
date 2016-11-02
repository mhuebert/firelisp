(ns test.cards
  (:require [devcards.core :as devcards :include-macros true]
            [static.targaryen]
            [firelisp.tests.db]
            [firelisp.tests.ruleset]
            [firelisp.tests.emit]
            [firelisp.tests.standard-lib]
            [firelisp.tests.targaryen]
            [firelisp.tests.examples]))

(devcards/start-devcard-ui!)