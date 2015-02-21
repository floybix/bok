(ns org.nfrac.bok.examples.humanoid2
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.gait
             :refer [eval-gait grounded? humanoid-walk]]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [x-val y-val abs
                     turn-towards v-angle v-sub HALF_PI]]))

(def ident {:creature-type :humanoid
            :name "Balanced walking gait, adapted from Yin et al (2007) SIMBICON."
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

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

(defn my-action-fn-opp
  [state]
  (let [{:keys [entities my-key other-players]} (:current state)
        opp-key (first other-players)
        me (get entities my-key)
        ;; merge joint info into corresponding named components:
        {:keys [head torso arm-a1 arm-a2 arm-b1 arm-b2
                leg-a1 leg-a2 leg-a3 leg-b1 leg-b2 leg-b3]}
        (merge-with merge ;; merge nested maps
                    (:components me)
                    (:joints me))

        eye (first (:points head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        dir (if (< (x-val (:position opp-eye))
                   (x-val (:position eye))) -1 1)
        legspan (->> [leg-a1 leg-a2 leg-a3 leg-b1 leg-b2 leg-b3]
                     (map x-offset))
        balance? (and (> (height head) (+ (height leg-a2) 1.5))
                      (> (height head) (+ (height leg-b2) 1.5))
                      (< (apply min legspan)
                         (- 0.15 (x-offset head))
                         (+ 0.15 (x-offset head))
                         (apply max legspan)))
        react-arms (if (or (grounded? arm-a1)
                           (grounded? arm-b1)
                           (grounded? arm-a2)
                           (grounded? arm-b2))
                     {:arm-a1 (turn-towards 0 (:angle arm-a1) (:joint-speed arm-a1) 12)
                      :arm-b1 (turn-towards 0 (:angle arm-b1) (:joint-speed arm-b1) 12)
                      :arm-a2 (turn-towards 0 (:angle arm-a2) (:joint-speed arm-a2) 12)
                      :arm-b2 (turn-towards 0 (:angle arm-b2) (:joint-speed arm-b2) 12)}
                     {:arm-a1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-a1) 8)
                      :arm-b1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-b1) 8)
                      :arm-a2 (turn-towards 0 (:joint-angle arm-a2) (:joint-speed arm-a2) 5)
                      :arm-b2 (turn-towards 0 (:joint-angle arm-b2) (:joint-speed arm-b2) 5)})
        [actions* gait-state] (eval-gait me dir humanoid-walk (:gait-state state)
                                         (:time (:current state)))
        actions (if balance?
                  actions*
                  (merge actions* react-arms))]
    (print (str (:phase gait-state) " "))
    (flush)
    (assoc state
      :actions actions
      :gait-state gait-state)))

(defn my-action-fn
  [state]
  (let [{:keys [entities other-players]} (:current state)
        opp-key (first other-players)]
    (if-not (get entities opp-key)
      ;; can not see opponent. do nothing
      state
      (my-action-fn-opp state))))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
