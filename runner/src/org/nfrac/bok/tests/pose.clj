(ns org.nfrac.bok.tests.pose
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.game-arenas] ;; load games
            [org.nfrac.bok.visual-runner :as visrun]
            [org.nfrac.bok.runner :as runner]
            [clojure.pprint :refer [pprint]]))

;; max torque
(def MT 100.0)

(defn action
  [info]
  (let [{:keys [entities my-key other-players]} info
        me (get entities my-key)
        {:keys [head leg-a1 leg-a2 leg-b1 leg-b2]} (:components me)]
    {:joint-motors {:leg-b1 [(* 10 (- 2.0 (:angle leg-b1))) MT]
                    :leg-a2 [(* 10 (- -0.6 (:angle leg-a2))) MT]
                    :leg-b2 [(* 10 (- 0.6 (:angle leg-b2))) MT]}}))

(defn -main
  "Run the test sketch."
  [& [arena-type]]
  (let [arena-type (or (keyword arena-type) :sumo)
        game (games/build arena-type
                          {:player-a :bipoid
                           :player-b :bipoid}
                          {})]
    (-> game
        (visrun/run-with-display #(runner/step-local % {:player-a action
                                                        :player-b action}))
        :final-result
        pprint)))
