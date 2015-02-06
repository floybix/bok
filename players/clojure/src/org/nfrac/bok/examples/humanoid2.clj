(ns org.nfrac.bok.examples.humanoid2
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.gait
             :refer [symmetric-gait grounded? humanoid-walk]]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [abs sign x-val y-val angle-left?
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

(defn my-action-fn
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
        opp-ft (util/point-features opp-eye eye)
        opp-angle (:angle-from-me opp-ft)
        dir (if (angle-left? opp-angle) -1 1)

        ground-entities #{:ground}

        legspan (->> [leg-a1 leg-a2 leg-a3 leg-b1 leg-b2 leg-b3]
                     (map x-offset))
        balance? (and (> (height head) (+ (height leg-a2) 1.5))
                      (> (height head) (+ (height leg-b2) 1.5))
                      (< (apply min legspan)
                         (- 0.15 (x-offset head))
                         (+ 0.15 (x-offset head))
                         (apply max legspan)))
        react-arms (if (or (grounded? arm-a1 ground-entities)
                           (grounded? arm-b1 ground-entities)
                           (grounded? arm-a2 ground-entities)
                           (grounded? arm-b2 ground-entities))
                     {:arm-a1 (turn-towards 0 (:angle arm-a1) (:joint-speed arm-a1) 12)
                      :arm-b1 (turn-towards 0 (:angle arm-b1) (:joint-speed arm-b1) 12)
                      :arm-a2 (turn-towards 0 (:angle arm-a2) (:joint-speed arm-a2) 12)
                      :arm-b2 (turn-towards 0 (:angle arm-b2) (:joint-speed arm-b2) 12)}
                     {:arm-a1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-a1) 8)
                      :arm-b1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-b1) 8)
                      :arm-a2 (turn-towards 0 (:joint-angle arm-a2) (:joint-speed arm-a2) 5)
                      :arm-b2 (turn-towards 0 (:joint-angle arm-b2) (:joint-speed arm-b2) 5)})

        curr-op (:operator state 0)
        curr-elapsed (- (:time (:current state))
                        (:operator-start state 0))
        gait humanoid-walk
        [op actions elapsed] (loop [op curr-op
                                    elapsed curr-elapsed]
                               (if-let [actions* (symmetric-gait me dir gait op elapsed
                                                                 ground-entities)]
                                 (let [actions (if balance?
                                                 actions*
                                                 (merge actions* react-arms))]
                                   [op actions elapsed])
                                 (recur (mod (inc op) (* 2 (count gait)))
                                        0.0)))]
    (print (str op " "))
    (flush)
    (assoc state
      :actions actions
      :operator op
      :operator-start (- (:time (:current state)) elapsed))))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
