(ns org.nfrac.hatto.tests.hatto
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.hatto.visual-runner :as visrun]
            [cljbox2d.core :refer [step! mass]]
            [quil.core :as quil]
            [clojure.pprint :refer [pprint]]))

(defn a-action
  [info]
  {:limb-b-rj 1
   :limb-ba-rj 2
   :limb-baa-rj 3
   :limb-a-rj -1
   :limb-aa-rj 2
   :limb-aaa-rj 3
   })

(defn b-action
  [info]
  ;(clojure.pprint/pprint info)
  {:limb-a-rj 0
   :limb-b-rj 2
   :limb-aa-rj -5
   :limb-ab-rj 0
   :limb-aaa-rj 1
   :limb-aab-rj 1
   :limb-aba-rj 1
   :limb-abb-rj 1})

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
  (let [game (core/setup-game :simple :hatto :hatto)]
    (println "hatto mass:"
             (-> game :entities :creature-a core/entity-mass))
    (-> game
        (visrun/run-with-display step-local)
        :final-result
        pprint)))
