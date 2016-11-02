(defproject org.clojars.mhuebert/firelisp "0.1.0-SNAPSHOT"
  :description "Use Clojure(Script) to write Firebase rules"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.5.3"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [devcards "0.2.1-7"]
                 [sablono "0.7.1"]

                 [cljsjs/react "15.0.2-0"]
                 [cljsjs/react-dom "15.0.2-0"]
                 [cljsjs/react-dom-server "15.0.2-0"]

                 [doo "0.1.7"]]

  :npm {:dependencies [["targaryen" "mhuebert/targaryen#ef54563c"]
                       ["browserify" "13.1.0"]]
        :package      {:scripts {:postinstall "mkdir src/js; browserify node_modules/targaryen/index.js -s targaryen -o src/js/targaryen.js;"}}}

  :plugins [[lein-figwheel "0.5.3-2"]
            [lein-npm "0.6.2"]
            [lein-doo "0.1.7"]
            [lein-cljsbuild "1.1.3" :exclusions [org.clojure/clojure]]]

  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    "target"]

  :source-paths ["src"]

  :doo {:build "test"}

  :cljsbuild {:builds [{:id           "devcards"
                        :source-paths ["src" "tests"]
                        :figwheel     {:devcards true}
                        :compiler     {:main                 "test.runner"
                                       :asset-path           "js/compiled/devcards_out"
                                       :output-to            "resources/public/js/compiled/firelisp_devcards.js"
                                       :output-dir           "resources/public/js/compiled/devcards_out"
                                       :source-map-timestamp true}}
                       {:id           "test"
                        :source-paths ["src" "tests"]
                        :compiler     {:main                 "test.runner"
                                       :asset-path           "js/compiled/test_out"
                                       :output-to            "target/test.js"
                                       :output-dir           "target/test_out"
                                       :source-map-timestamp true
                                       :parallel-build       true
                                       :optimizations        :simple}}
                       {:id           "prod"
                        :source-paths ["src"]
                        :compiler     {:main          "firelisp.core"
                                       :asset-path    "js/compiled/out"
                                       :output-to     "resources/public/js/compiled/firelisp.js"
                                       :optimizations :advanced}}]}

  :figwheel {:css-dirs    ["resources/public/css"]
             :server-port 3451})
