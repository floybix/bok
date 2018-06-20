(defproject org.nfrac/bok.cljplayer "0.1.0-SNAPSHOT"
  :description "For writing Bok players in Clojure."
  :url "https://github.com/floybix/bok"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}
  )
