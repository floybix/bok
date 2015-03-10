(defproject org.nfrac/bok "0.1.0-SNAPSHOT"
  :description "Run Bok bouts either visually or in batch mode."
  :url "https://github.com/floybix/bok"
  :scm {:name "git"
        :url "https://github.com/floybix/bok"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.nfrac/cljbox2d "0.5.0"]
                 [org.nfrac/cljbox2d.testbed "0.5.0"]
                 [org.zeromq/jeromq "0.3.4"]
                 [com.cognitect/transit-clj "0.8.259"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/clojure "1.6.0"]]
  :main org.nfrac.bok.main
  ;:jvm-opts ^:replace ["-server" "-XX:+UseConcMarkSweepGC"]
  )
