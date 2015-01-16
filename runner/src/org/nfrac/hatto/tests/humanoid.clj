(ns org.nfrac.hatto.tests.humanoid
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.hatto.visual-runner :as visrun]
            [cljbox2d.core :refer [step! mass]]
            [quil.core :as quil]
            [clojure.pprint :refer [pprint]]))

(defn a-action
  [info]
  {:arm-a1-rj -10
   :arm-b1-rj -2
   :leg-a2-rj 5
   :leg-b2-rj 2})

(defn b-action
  [info]
  {})

(defn step-local
  [game]
  (if (:paused? game)
    game
    (if-let [res (core/final-result game)]
      (do
        (quil/exit)
        (assoc game :final-result res))
      (-> game
          (update-in [:world] step! (:dt-secs game))
          (update-in [:time] + (:dt-secs game))
          (core/take-actions a-action b-action)))))

(defn -main
  "Run the test sketch."
  [& args]
  (let [game (core/setup-game :simple :humanoid :humanoid)]
    (println "humanoid mass:"
             (-> game :entities :creature-a core/entity-mass))
    (-> game
        (visrun/run-with-display step-local)
        :final-result
        pprint)))
