(ns org.nfrac.bok.tests.wormoid
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.visual-runner :as visrun]
            [org.nfrac.bok.runner :as runner]
            [org.nfrac.bok.entities :as ent]
            [clojure.pprint :refer [pprint]]))

;; max torque
(def MT 50.0)

(defn a-action
  [info]
  {:joint-motors {:seg-1 [8 MT]
                  :seg-2 [0 MT]
                  :seg-5 [5 MT]}
   :raycast Math/PI
   :gun {:fire true
         :speed 1}})

(defn b-action
  [info]
  {:joint-motors {:seg-2 [-10 MT]
                  :seg-3 [-5 MT]}
   :gun {:fire true
         :speed 0.02}})

(defn -main
  "Run the test sketch."
  [& [arena-type]]
  (let [arena-type (or (keyword arena-type) :sumo)
        game (games/build arena-type
                          {:player-a :wormoid
                           :player-b :wormoid}
                          {})]
    (println "wormoid mass:"
             (-> game :entities :player-a ent/entity-mass))
    (-> game
        (visrun/run-with-display #(runner/step-local % {:player-a a-action
                                                        :player-b b-action}))
        :final-result
        pprint)))
