(ns org.nfrac.bok.examples.humanoid1
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [abs angle-left? angle-up? turn-towards HALF_PI]]))

(def ident {:creature-type :humanoid
            :name "Example humanoid1"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

(def UP HALF_PI)
(def DOWN (- HALF_PI))
(def LEFT Math/PI)
(def RIGHT 0.0)

(defn my-action-fn
  [state]
  (let [{:keys [my-key entities]} (:current state)
        [opp-key] (keys (dissoc entities my-key :arena))
        me (get entities my-key)
        my-head (:head (:components me))
        eye (first (:points my-head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        opp-ft (util/point-features opp-eye eye)
        opp-angle (:angle-from-me opp-ft)
        dir (if (angle-left? opp-angle) 1 -1)
        l-a2-angle (-> me :components :leg-a2 :angle)
        l-b2-angle (-> me :components :leg-b2 :angle)
        a-a2-angle (-> me :components :arm-a2 :angle)
        a-b2-angle (-> me :components :arm-b2 :angle)
        a1-sp (* 5 dir)
        b1-sp (* 4 dir)
        l-a2-sp (turn-towards DOWN l-a2-angle 40)
        l-b2-sp (turn-towards DOWN l-b2-angle 40)
        a-a2-sp (turn-towards DOWN a-a2-angle 40)
        a-b2-sp (turn-towards DOWN a-b2-angle 40)
        actions {:joints {:leg-a1-rj a1-sp
                          :leg-b1-rj b1-sp
                          :leg-a2-rj l-a2-sp
                          :leg-b2-rj l-b2-sp
                          :arm-a1-rj a1-sp
                          :arm-b1-rj b1-sp
                          :arm-a2-rj a-a2-sp
                          :arm-b2-rj a-b2-sp
                          :pelvis-rj 0
                          }}]
    (assoc state
      :actions actions)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
