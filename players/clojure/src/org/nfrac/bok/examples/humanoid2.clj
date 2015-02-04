(ns org.nfrac.bok.examples.humanoid2
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [abs sign x-val y-val angle-left? angle-right? angle-up?
                     turn-towards v-angle v-sub HALF_PI]]))

(def ident {:creature-type :humanoid
            :name "Balanced walking gait, adapted from Yin et al (2007) SIMBICON."
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

(def UP HALF_PI)
(def DOWN (- HALF_PI))
(def LEFT Math/PI)
(def RIGHT 0.0)

(defn height
  [component]
  (y-val (:position (first (:points component)))))

(defn x-offset
  [component]
  (x-val (:position (first (:points component)))))

(defn midpoint
  [xs]
  (let [maxx (apply max xs)
        minx (apply min xs)]
    (+ minx (* 0.5 (- maxx minx)))))

(defn my-action-fn
  [state]
  (let [{:keys [my-key entities]} (:current state)
        [opp-key] (keys (dissoc entities my-key :arena))
        me (get entities my-key)
        ;; merge joint info into corresponding named components:
        {:keys [head torso
                arm-a1 arm-a2 arm-b1 arm-b2
                leg-a1 leg-a2 leg-a3 leg-b1 leg-b2 leg-b3]}
        (merge-with merge ;; merge nested maps
                    (:components me)
                    (:joints me))

        eye (first (:points head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        opp-ft (util/point-features opp-eye eye)
        opp-angle (:angle-from-me opp-ft)
        dir (if (angle-left? opp-angle) -1 1)

        FOOT_FWD (* dir (/ Math/PI 2))
        foot-a-grounded? (or (< (height leg-a2) 0.06)
                             (< (height leg-a3) 0.06))
        foot-b-grounded? (or (< (height leg-b2) 0.06)
                             (< (height leg-b3) 0.06))

        com (:center-of-mass me)
        comv (/ (reduce + (map (comp x-val :velocity) (:points torso))) 2)

        support (->> [leg-a2 leg-a3 leg-b2 leg-b3]
                     (filter #(< (height %) 0.06))
                     (map x-offset))
        cos (if (seq support)
              (midpoint support)
              (x-val com))

        legspan (->> [leg-a1 leg-a2 leg-a3 leg-b1 leg-b2 leg-b3]
                     (map x-offset))
        balance? (< (apply min legspan) (x-offset head) (apply max legspan))
        react-arms {
              :arm-a1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-a1) 8)
              :arm-b1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-b1) 8)
              :arm-a2 (turn-towards DOWN (:angle arm-a2) (:joint-speed arm-a2) 2)
              :arm-b2 (turn-towards DOWN (:angle arm-b2) (:joint-speed arm-b2) 2)
              }

        lsp 8
        fsp 4
        asp 2
        akd 40
        akv 4

        ;; finite state machine, each a function returning actions or
        ;; nil to relinquish control

        fsm-ops
        [ ;; 0 - begin leg a stance, leg b swing
         (fn [time-in-state]
           (when (< time-in-state 0.3)
             (let [ ;; balance feedback to swing hip
                   c_d 0.0
                   c_v 0.2
                   d (- (x-val com) (x-offset leg-a2))
                   adj (+ (* c_d d) (* c_v comv))]
               {:joint-motors
                {
                 ;; stance hip to balance torso upright (reverse joint)
                 :leg-a1 (turn-towards 0 (- (:angle torso)) (:joint-speed leg-a1) lsp)
                 :leg-a2 (turn-towards -0.05 (:joint-angle leg-a2) (:joint-speed leg-a2) lsp)
                 :leg-a3 (turn-towards (+ FOOT_FWD 0.00) (:joint-angle leg-a3) (:joint-speed leg-a3) fsp)
                 ;; swing leg
                 :leg-b1 (turn-towards (+ DOWN 0.50 adj) (:angle leg-b1) (:joint-speed leg-b1) lsp)
                 :leg-b2 (turn-towards -1.10 (:joint-angle leg-b2) (:joint-speed leg-b2) lsp)
                 :leg-b3 (turn-towards (+ FOOT_FWD 0.20) (:joint-angle leg-b3) (:joint-speed leg-b3) fsp)
                 ;; arms
                 :arm-a1 (turn-towards (+ DOWN -0.30) (:joint-angle arm-a1) (:joint-speed arm-a1) asp akd akv)
                 :arm-b1 (turn-towards (+ DOWN 0.30) (:joint-angle arm-b1) (:joint-speed arm-b1) asp akd akv)
                 :arm-a2 (turn-towards 0.40 (:joint-angle arm-a2) (:joint-speed arm-a2) asp akd akv)
                 :arm-b2 (turn-towards 0.40 (:joint-angle arm-b2) (:joint-speed arm-b2) asp akd akv)
                 }})))
         ;; 1 - complete leg a stance, leg b swing
         (fn [time-in-state]
           ;; until b foot strike
           (when (and (not foot-b-grounded?)
                      ;; swing leg still ahead
                      (pos? (* dir (- (:angle leg-b1) DOWN))))
             (let [ ;; balance feedback to swing hip
                   c_d 2.0
                   c_v 0.0
                   d (- (x-val com) (x-offset leg-a2))
                   adj (+ (* c_d d) (* c_v comv))]
               {:joint-motors
                {
                 ;; stance hip to balance torso upright (reverse joint)
                 :leg-a1 (turn-towards 0 (- (:angle torso)) (:joint-speed leg-a1) lsp)
                 :leg-a2 (turn-towards -0.10 (:joint-angle leg-a2) (:joint-speed leg-a2) lsp)
                 :leg-a3 (turn-towards (+ FOOT_FWD 0.20) (:joint-angle leg-a3) (:joint-speed leg-a3) fsp)
                 ;; swing leg
                 :leg-b1 (turn-towards (+ DOWN -0.70 adj) (:angle leg-b1) (:joint-speed leg-b1) lsp)
                 :leg-b2 (turn-towards -0.05 (:joint-angle leg-b2) (:joint-speed leg-b2) lsp)
                 :leg-b3 (turn-towards (+ FOOT_FWD 0.00) (:joint-angle leg-b3) (:joint-speed leg-b3) fsp)
                 ;; arms
                 :arm-a1 (turn-towards (+ DOWN 0.30) (:joint-angle arm-a1) (:joint-speed arm-a1) asp akd akv)
                 :arm-b1 (turn-towards (+ DOWN -0.30) (:joint-angle arm-b1) (:joint-speed arm-b1) asp akd akv)
                 :arm-a2 (turn-towards 0.40 (:joint-angle arm-a2) (:joint-speed arm-a2) asp akd akv)
                 :arm-b2 (turn-towards 0.40 (:joint-angle arm-b2) (:joint-speed arm-b2) asp akd akv)
                 }})))
         ;; 2 - begin leg b stance, leg a swing
         (fn [time-in-state]
           (when (< time-in-state 0.3)
             (let [ ;; balance feedback to swing hip
                   c_d 0.0
                   c_v 0.2
                   d (- (x-val com) (x-offset leg-a2))
                   adj (+ (* c_d d) (* c_v comv))]
               {:joint-motors
                {
                 ;; swing leg
                 :leg-a1 (turn-towards (+ DOWN 0.50 adj) (:angle leg-a1) (:joint-speed leg-a1) lsp)
                 :leg-a2 (turn-towards -1.10 (:joint-angle leg-a2) (:joint-speed leg-a2) lsp)
                 :leg-a3 (turn-towards (+ FOOT_FWD 0.20) (:joint-angle leg-a3) (:joint-speed leg-a3) fsp)
                 ;; stance hip to balance torso upright (reverse joint)
                 :leg-b1 (turn-towards 0 (- (:angle torso)) (:joint-speed leg-b1) lsp)
                 :leg-b2 (turn-towards -0.05 (:joint-angle leg-b2) (:joint-speed leg-b2) lsp)
                 :leg-b3 (turn-towards (+ FOOT_FWD 0.00) (:joint-angle leg-b3) (:joint-speed leg-b3) fsp)
                 ;; arms
                 :arm-a1 (turn-towards (+ DOWN 0.30) (:joint-angle arm-a1) (:joint-speed arm-a1) asp akd akv)
                 :arm-b1 (turn-towards (+ DOWN -0.30) (:joint-angle arm-b1) (:joint-speed arm-b1) asp akd akv)
                 :arm-a2 (turn-towards 0.40 (:joint-angle arm-a2) (:joint-speed arm-a2) asp akd akv)
                 :arm-b2 (turn-towards 0.40 (:joint-angle arm-b2) (:joint-speed arm-b2) asp akd akv)
                 }})))
         ;; 3 - complete leg b stance, leg a swing
         (fn [time-in-state]
           ;; until a foot strike
           (when (and (not foot-a-grounded?)
                      ;; swing leg still ahead
                      (pos? (* dir (- (:angle leg-a1) DOWN))))
             (let [ ;; balance feedback to swing hip
                   c_d 2.0
                   c_v 0.0
                   d (- (x-val com) (x-offset leg-b2))
                   adj (+ (* c_d d) (* c_v comv))]
               {:joint-motors
                {
                 ;; swing leg
                 :leg-a1 (turn-towards (+ DOWN -0.70 adj) (:angle leg-a1) (:joint-speed leg-a1) lsp)
                 :leg-a2 (turn-towards -0.05 (:joint-angle leg-a2) (:joint-speed leg-a2) lsp)
                 :leg-a3 (turn-towards (+ FOOT_FWD 0.00) (:joint-angle leg-a3) (:joint-speed leg-a3) fsp)
                 ;; stance hip to balance torso upright (reverse joint)
                 :leg-b1 (turn-towards 0 (- (:angle torso)) (:joint-speed leg-b1) lsp)
                 :leg-b2 (turn-towards -0.10 (:joint-angle leg-b2) (:joint-speed leg-b2) lsp)
                 :leg-b3 (turn-towards (+ FOOT_FWD 0.20) (:joint-angle leg-b3) (:joint-speed leg-b3) fsp)
                 ;; arms
                 :arm-a1 (turn-towards (+ DOWN -0.30) (:joint-angle arm-a1) (:joint-speed arm-a1) asp akd akv)
                 :arm-b1 (turn-towards (+ DOWN 0.30) (:joint-angle arm-b1) (:joint-speed arm-b1) asp akd akv)
                 :arm-a2 (turn-towards 0.40 (:joint-angle arm-a2) (:joint-speed arm-a2) asp akd akv)
                 :arm-b2 (turn-towards 0.40 (:joint-angle arm-b2) (:joint-speed arm-b2) asp akd akv)
                 }})))
         ]
        curr-op (:operator state 0)
        curr-elapsed (- (:time (:current state))
                        (:operator-start state 0))
        [next-op actions elapsed] (loop [op curr-op
                                         elapsed curr-elapsed]
                                    (let [op-fn (get fsm-ops op)]
                                      (if-let [actions* (op-fn elapsed)]
                                        (let [actions (if balance?
                                                        actions*
                                                        (merge actions* react-arms))]
                                          [op actions elapsed])
                                        (recur (mod (inc op) (count fsm-ops))
                                               0.0))))
        ]
    (print (str next-op " "))
    (flush)
    (assoc state
      :actions actions
      :operator next-op
      :operator-start (if (= curr-op next-op)
                        (:operator-start state 0)
                        (:time (:current state))))))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
