(ns org.nfrac.hatto.tests.legge
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.hatto.players :as players]
            [org.nfrac.hatto.arena-simple :as arenas]
            [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer [new-world step!]]
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

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        arena (arenas/build! world)
        player-a (->
                  (players/legge world [-10 10] -1)
                  (assoc :action-fn a-action))
        player-b (->
                  (players/legge world [10 10] -2)
                  (assoc :action-fn b-action))]
    (assoc bed/initial-state
      :world world
      :camera {:width 40 :height 20 :x-left -20 :y-bottom -5}
      :game {:arena arena
             :player-a player-a
             :player-b player-b})))

(defn step
  [state]
  (if (:paused? state)
    state
    (-> (update-in state [:world] step! (:dt-secs state))
        (core/post-step))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Hatto"
    :setup setup
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [1200 600]
    :middleware [quil.middleware/fun-mode]))
