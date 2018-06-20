(defproject org.nfrac/bok "0.1.0-SNAPSHOT"
  :description "Run Bok bouts either visually or in batch mode."
  :url "https://github.com/floybix/bok"
  :scm {:name "git"
        :url "https://github.com/floybix/bok"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :main org.nfrac.bok.main
  ;:jvm-opts ^:replace ["-server" "-XX:+UseConcMarkSweepGC"]
  )
