(ns org.nfrac.hatto.tests.legsoid
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.hatto.visual-runner :as visrun]
            [cljbox2d.core :refer [step! mass]]
            [quil.core :as quil]
            [clojure.pprint :refer [pprint]]))

(defn a-action
  [info]
  {:limb-a1-rj -10
   :limb-b1-rj -2
   :limb-a2-rj 5
   :limb-b2-rj 2})

(defn b-action
  [info]
  {:limb-a1-rj 5
   :limb-b1-rj 0
   :limb-a2-rj -8})

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
  (let [game (core/setup-game :simple :legsoid :legsoid)]
    (println "legsoid mass:"
             (-> game :entities :creature-a core/entity-mass))
    (-> game
        (visrun/run-with-display step-local)
        :final-result
        pprint)))
