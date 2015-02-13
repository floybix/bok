(ns org.nfrac.bok.tests.bipoid
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.visual-runner :as visrun]
            [org.nfrac.bok.runner :as runner]
            [org.nfrac.bok.entities :as ent]
            [clojure.pprint :refer [pprint]]))

;; max torque
(def MT 50.0)

(defn a-action
  [info]
  {:joint-motors {:leg-a1 [-10 MT]
                  :leg-b1 [-2 MT]
                  :leg-a2 [5 MT]
                  :leg-b2 [2 MT]}
   :gun {:fire true
         :speed 1}})

(defn b-action
  [info]
  {:joint-motors {:leg-a1 [5 MT]
                  :leg-b1 [5 MT]
                  :leg-a2 [-8 MT]}
   :gun {:fire true
         :speed 0.02}})

(defn -main
  "Run the test sketch."
  [& [arena-type]]
  (let [arena-type (or (keyword arena-type) :sumo)
        game (games/build arena-type
                          {:player-a :bipoid
                           :player-b :bipoid}
                          {})]
    (-> game
        (visrun/run-with-display #(runner/step-local % {:player-a a-action
                                                        :player-b b-action}))
        :final-result
        pprint)))
