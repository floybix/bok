(defproject org.nfrac/hatto "0.1.0-SNAPSHOT"
  :description "Run Hatto bouts either visually or in batch mode."
  :url "https://github.com/floybix/hatto"
  :scm {:name "git"
        :url "https://github.com/floybix/hatto"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.nfrac/cljbox2d "0.3.0"]
                 [org.nfrac/cljbox2d.testbed "0.3.0"]
                 [org.zeromq/jeromq "0.3.4"]
                 [com.cognitect/transit-clj "0.8.259"]
                 [org.clojure/clojure "1.6.0"]]
  ;:jvm-opts ^:replace ["-server" "-XX:+UseConcMarkSweepGC"]
  )
