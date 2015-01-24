(ns org.nfrac.hatto.tests.wormoid
  (:require [org.nfrac.hatto.games :as games]
            [org.nfrac.hatto.visual-runner :as visrun]
            [org.nfrac.hatto.runner :as runner]
            [org.nfrac.hatto.entities :as ent]
            [clojure.pprint :refer [pprint]]))

(defn a-action
  [info]
  {:joints {:seg-1-rj 8
            :seg-2-rj 0
            :seg-5-rj 5}
   :gun {:fire true
         :speed 1}})

(defn b-action
  [info]
  {:joints {:seg-2-rj -10
            :seg-3-rj -5}
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
