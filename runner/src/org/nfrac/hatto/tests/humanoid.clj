(ns org.nfrac.hatto.tests.humanoid
  (:require [org.nfrac.hatto.games :as games]
            [org.nfrac.hatto.visual-runner :as visrun]
            [org.nfrac.hatto.entities :as ent]
            [clojure.pprint :refer [pprint]]))

(defn a-action
  [info]
  {:joints {:arm-a1-rj -10
            :arm-b1-rj -2
            :leg-a2-rj 5
            :leg-b2-rj 2}})

(defn b-action
  [info]
  {})

(defn -main
  "Run the test sketch."
  [& [arena-type]]
  (let [arena-type (or (keyword arena-type) :sumo)
        game (games/build arena-type
                          {:player-a :humanoid
                           :player-b :humanoid}
                          {})]
    (println "humanoid mass:"
             (-> game :entities :player-a ent/entity-mass))
    (-> game
        (visrun/run-with-display #(visrun/step-local % {:player-a a-action
                                                        :player-b b-action}))
        :final-result
        pprint)))
