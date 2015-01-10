(ns org.nfrac.hatto.tests.hatto
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer [step! mass]]
            [quil.core :as quil]
            [quil.middleware]
            [clojure.pprint]))

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

(defn step
  [state]
  (if (:paused? state)
    state
    (-> state
        (update-in [:world] step! (:dt-secs state))
        (update-in [:time] + (:dt-secs state))
        (core/take-actions a-action b-action))))

(defn setup []
  (quil/frame-rate 30)
  (let [game (core/setup-game :hatto :hatto)]
    (println "hatto mass:"
             (-> game :entities :creature-a core/entity-mass))
    (merge bed/initial-state game)))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Hatto hatto demo"
    :setup setup
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [1200 600]
    :middleware [quil.middleware/fun-mode]))
