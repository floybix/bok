(ns org.nfrac.bok.tests.three
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.game-arenas] ;; load games
            [org.nfrac.bok.visual-runner :as visrun]
            [org.nfrac.bok.runner :as runner]
            [org.nfrac.bok.entities :as ent]
            [clojure.pprint :refer [pprint]]))

(def MT 100)

(defn action
  [info]
  {:joint-motors {:wheel-a [0 MT]
                  :wheel-b [0 MT]}})

(defn -main
  "Run the test sketch."
  [& [arena-type]]
  (let [arena-type (or (keyword arena-type) :sumo)
        game (games/build arena-type
                          {:player-a :bipoid
                           :player-b :humanoid
                           :player-c :wormoid}
                          {})]
    (println "bipoid mass:"
             (-> game :entities :player-a ent/entity-mass))
    (println "humanoid mass:"
             (-> game :entities :player-b ent/entity-mass))
    (println "wormoid mass:"
             (-> game :entities :player-c ent/entity-mass))
    (-> game
        (visrun/run-with-display #(runner/step-local % {:player-a action
                                                        :player-b action
                                                        :player-c action}))
        :final-result
        pprint)))
