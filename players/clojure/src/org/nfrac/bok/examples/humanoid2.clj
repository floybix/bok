(ns org.nfrac.bok.examples.humanoid2
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [abs angle-left? angle-up? turn-towards HALF_PI]]))

(def ident {:creature-type :humanoid
            :name "Example humanoid2"
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
        ;; try to keep torso & head balanced
        t-ang (-> me :components :torso :angle)
        p-sp (turn-towards 0 t-ang 5)
        a-a1-angle (-> me :components :arm-a1 :angle)
        a-b1-angle (-> me :components :arm-b1 :angle)
        a-a2-angle (-> me :components :arm-a2 :angle)
        a-b2-angle (-> me :components :arm-b2 :angle)
        a-a2-sp 0;(turn-towards (- UP t-ang) a-a2-angle 40)
        a-b2-sp 0;(turn-towards (- UP t-ang) a-b2-angle 40)
        a-a1-sp (turn-towards (+ (- DOWN 1.0) t-ang) a-a1-angle 20)
        a-b1-sp (turn-towards (+ (+ DOWN 1.0) t-ang) a-b1-angle 20)
        ;; walk legs
        l-a1-angle (-> me :components :leg-a1 :angle)
        l-b1-angle (-> me :components :leg-b1 :angle)
        l-a2-angle (-> me :components :leg-a2 :angle)
        l-b2-angle (-> me :components :leg-b2 :angle)
        l-a1-sp (turn-towards (- (- DOWN 0.5) (max 0 t-ang)) l-a1-angle 20) ;(* 5 dir)
        l-b1-sp (turn-towards (- (+ DOWN 0.5) (min 0 t-ang)) l-b1-angle 20) ;(* 4 dir)
        l-a2-sp (turn-towards DOWN l-a2-angle 20)
        l-b2-sp (turn-towards DOWN l-b2-angle 20)
        actions {:joints {:leg-a1-rj l-a1-sp
                          :leg-b1-rj l-b1-sp
                          :leg-a2-rj l-a2-sp
                          :leg-b2-rj l-b2-sp
                          :arm-a1-rj a-a1-sp
                          :arm-b1-rj a-b1-sp
                          :arm-a2-rj a-a2-sp
                          :arm-b2-rj a-b2-sp
                          :pelvis-rj p-sp
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
