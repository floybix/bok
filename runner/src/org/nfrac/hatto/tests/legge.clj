(ns org.nfrac.hatto.tests.legge
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer [step! mass]]
            [quil.core :as quil]
            [quil.middleware]
            [clojure.pprint]))

(defn a-action
  [info]
  ;(clojure.pprint/pprint info)
  {:limb-a1-rj 0
   :limb-b1-rj 0
   :limb-a2-rj -5
   :limb-b2-rj 0})

(defn b-action
  [info]
  {:limb-a1-rj 5
   :limb-b1-rj 0})

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
  (let [game (core/setup-game :legge :legge)]
    (println "legge mass:"
             (-> game :entities :creature-a core/entity-mass))
    (merge bed/initial-state game)))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Hatto legge demo"
    :setup setup
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [1200 600]
    :middleware [quil.middleware/fun-mode]))
