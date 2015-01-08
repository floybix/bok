(defproject org.nfrac/hatto "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.nfrac/cljbox2d "0.3.0-SNAPSHOT"]
                 [org.nfrac/cljbox2d.testbed "0.3.0-SNAPSHOT"]
                 [org.zeromq/jeromq "0.3.4"]
                 [org.zeromq/cljzmq "0.1.4" :exclusions [org.zeromq/jzmq]]
                 [com.cognitect/transit-clj "0.8.259"]
                 [org.clojure/clojure "1.6.0"]])
