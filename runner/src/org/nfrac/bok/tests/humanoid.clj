(ns org.nfrac.bok.tests.humanoid
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.visual-runner :as visrun]
            [org.nfrac.bok.runner :as runner]
            [org.nfrac.bok.entities :as ent]
            [clojure.pprint :refer [pprint]]))

;; max torque
(def MT 100.0)

(defn a-action
  [info]
  {:joint-motors {:arm-a1 [-4 MT]
                  :arm-b1 [-5 MT]
                  :arm-a2 [0 MT]
                  :arm-b2 [0 MT]
                  :leg-a1 [0 MT]
                  :leg-b1 [0 MT]
                  :leg-a2 [-5 MT]
                  :leg-b2 [-5 MT]
                  }})

(defn b-action
  [info]
  {:joint-motors {:arm-a1 [5 MT]
                  :arm-b1 [-3 MT]
                  :leg-a1 [5 MT]
                  :leg-b1 [-3 MT]
                  :leg-a2 [0 MT]
                  :leg-b2 [0 MT]}})

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
        (visrun/run-with-display #(runner/step-local % {:player-a a-action
                                                        :player-b b-action}))
        :final-result
        pprint)))
