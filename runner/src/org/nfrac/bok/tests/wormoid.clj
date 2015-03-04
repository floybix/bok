(ns org.nfrac.bok.tests.wormoid
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.game-arenas] ;; load games
            [org.nfrac.bok.visual-runner :as visrun]
            [org.nfrac.bok.runner :as runner]
            [clojure.pprint :refer [pprint]]))

;; max torque
(def MT 50.0)

(defn a-action
  [info]
  {:joint-motors {:seg-2 [8 MT]
                  :seg-3 [0 MT]
                  :seg-5 [5 MT]
                  :seg-7 [5 MT]}
   :raycast [-2.9 -3.0 -3.1 -3.2]
   :gun {:fire true
         :speed 1}})

(defn b-action
  [info]
  {:joint-motors {:seg-3 [-10 MT]
                  :seg-4 [-5 MT]
                  :seg-5 [5 MT]}
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
    (-> game
        (visrun/run-with-display #(runner/step-local % {:player-a a-action
                                                        :player-b b-action}))
        :final-result
        pprint)))
