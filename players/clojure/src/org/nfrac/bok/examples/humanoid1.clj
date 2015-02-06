(ns org.nfrac.bok.examples.humanoid1
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [x-val y-val abs angle-left? turn-towards HALF_PI]]))

(def ident {:creature-type :humanoid
            :name "Example - tumbler"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

;; max torque
(def MT 100.0)

(defn my-action-fn
  [state]
  (let [{:keys [entities my-key other-players]} (:current state)
        opp-key (first other-players)
        me (get entities my-key)
        {:keys [head torso arm-a1 arm-a2 arm-b1 arm-b2
                leg-a1 leg-a2 leg-b1 leg-b2 leg-a3 leg-b3]} (:components me)
        eye (first (:points head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        opp-ft (util/point-features opp-eye eye)
        opp-angle (:angle-from-me opp-ft)
        dir (if (angle-left? opp-angle) -1 1)
        BRACE -0.5
        a1-sp (* 8 dir)
        b1-sp (* 7 dir)
        x-vel (x-val (:velocity me))
        actions
        (if (< (* dir x-vel) -1)
          ;; if heading the wrong way, just collapse
          {:joint-motors
           {}}
          {:joint-motors
           {:leg-a1 [a1-sp MT]
            :leg-b1 [b1-sp MT]
            :leg-a2 [(* 20 (- (:angle leg-a2))) MT]
            :leg-b2 (turn-towards (* dir BRACE) (:angle leg-b2) 0 8)
            :leg-a3 (turn-towards (* dir HALF_PI) (:angle leg-a3) 0 8)
            :leg-b3 (turn-towards (* dir HALF_PI) (:angle leg-b3) 0 8)
            :arm-a1 [a1-sp MT]
            :arm-b1 [b1-sp MT]
            :arm-a2 [(* 20 (- (:angle arm-a2))) MT]
            :arm-b2 (turn-towards (* dir BRACE) (:angle arm-b2) 0 8)
            }})]
    (assoc state
      :actions actions)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
