(ns org.nfrac.bok.tests.legsoid
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.visual-runner :as visrun]
            [org.nfrac.bok.runner :as runner]
            [org.nfrac.bok.entities :as ent]
            [clojure.pprint :refer [pprint]]))

(defn a-action
  [info]
  ;(pprint info)
  {:joints {:limb-a1-rj -10
            :limb-b1-rj -2
            :limb-a2-rj 5
            :limb-b2-rj 2}
   :gun {:fire true
         :speed 1}})

(defn b-action
  [info]
  {:joints {:limb-a1-rj 5
            :limb-b1-rj 0
            :limb-a2-rj -8}
   :gun {:fire true
         :speed 0.02}})

(defn -main
  "Run the test sketch."
  [& [arena-type]]
  (let [arena-type (or (keyword arena-type) :sumo)
        game (games/build arena-type
                          {:player-a :legsoid
                           :player-b :legsoid}
                          {})]
    (println "legsoid mass:"
             (-> game :entities :player-a ent/entity-mass))
    (-> game
        (visrun/run-with-display #(runner/step-local % {:player-a a-action
                                                        :player-b b-action}))
        :final-result
        pprint)))
