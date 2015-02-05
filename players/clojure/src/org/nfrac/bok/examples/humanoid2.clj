(ns org.nfrac.bok.examples.humanoid2
  (:require [org.nfrac.bok.cljplayer :as serv]
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

        foot-grounded? {:a (or (< (height leg-a2) 0.06)
                               (< (height leg-a3) 0.06))
                        :b (or (< (height leg-b2) 0.06)
                               (< (height leg-b3) 0.06))}

        com (:center-of-mass me)
        comv (/ (reduce + (map (comp x-val :velocity) (:points torso))) 2)

        legspan (->> [leg-a1 leg-a2 leg-a3 leg-b1 leg-b2 leg-b3]
                     (map x-offset))
        balance? (and (> (height head) (+ (height leg-a2) 1.5))
                      (> (height head) (+ (height leg-b2) 1.5))
                      (< (apply min legspan)
                         (- 0.15 (x-offset head))
                         (+ 0.15 (x-offset head))
                         (apply max legspan)))
        react-arms {
                    :arm-a1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-a1) 8)
                    :arm-b1 (turn-towards 0 (- (:angle torso)) (:joint-speed arm-b1) 8)
                    :arm-a2 (turn-towards 0 (:joint-angle arm-a2) (:joint-speed arm-a2) 6)
                    :arm-b2 (turn-towards 0 (:joint-angle arm-b2) (:joint-speed arm-b2) 6)
                    }

        ;; op 0 ==> stage 0, stance a, swing b
        ;; op 1 ==> stage 1, stance a, swing b
        ;; op 2 ==> stage 0, stance b, swing a
        ;; op 3 ==> stage 1, stance b, swing a
        walk [{:dt 0.3
               :c-d 0.0
               :c-v 0.2
               ;; this      \|/ is torso angle (hip joint conversely)
               :stance-leg [0.05 -0.05 (+ HALF_PI 0.0)]
               :swing-leg [0.5 -1.1 (+ HALF_PI 0.2)]
               :stance-arm [0.3 0.4]
               :swing-arm [-0.3 0.4]
               :leg-speed 8
               :arm-speed 2
               :arm-k-d 40
               :arm-k-v 4
               }
              {:dt :foot-contact
               :c-d 2.0
               :c-v 0.0
               ;; this      \|/ is torso angle (hip joint conversely)
               :stance-leg [0.05 -0.1 (+ HALF_PI 0.2)]
               :swing-leg [-0.05 -0.1 (+ HALF_PI 0.0)]
               :stance-arm [-0.3 0.4]
               :swing-arm [0.3 0.4]
               :leg-speed 8
               :arm-speed 2
               :arm-k-d 40
               :arm-k-v 4
               }]

        ;; finite state machine
        ;; returns actions map, or nil to relinquish control of current op

        symmetric-gait
        (fn [stages current-op time-in-op]
          (let [params (get stages (mod current-op (count stages)))
                stance (if (< current-op (count stages)) :a :b)
                swing (case stance :a :b, :b :a)
                leg-cmps {:a [:leg-a1 :leg-a2 :leg-a3]
                          :b [:leg-b1 :leg-b2 :leg-b3]}
                arm-cmps {:a [:arm-a1 :arm-a2]
                          :b [:arm-b1 :arm-b2]}
                ;; balance feedback to swing hip
                swing-leg2 (get-in me [:components (get-in leg-cmps [swing 1])])
                d (- (x-val com) (x-offset swing-leg2))
                adj (+ (* (:c-d params) d)
                       (* (:c-v params) comv))
                swing-leg1 (get-in me [:components (get-in leg-cmps [swing 0])])]
            (when-not (if (= :foot-contact (:dt params))
                        (or (foot-grounded? swing)
                             ;; swing foot already behind
                            (neg? (* dir (:angle swing-leg1))))
                        (>= time-in-op (:dt params)))
              (->>
               (for [limb [:stance-leg :swing-leg :stance-arm :swing-arm]
                     :let [leg? (contains? #{:stance-leg :swing-leg} limb)
                           limb-ks (case limb
                                     :stance-leg (get leg-cmps stance)
                                     :swing-leg (get leg-cmps swing)
                                     :stance-arm (get arm-cmps stance)
                                     :swing-arm (get arm-cmps swing))]
                     [i cmp-k theta] (map vector (range) limb-ks (get params limb))
                     :let [ang-vel (get-in me [:joints cmp-k :joint-speed])
                           curr-ang (cond
                                     ;; swing hip an absolute angle
                                     (and (= limb :swing-leg) (zero? i))
                                     (get-in me [:components cmp-k :angle])
                                     ;; stance hip to balance torso upright (reverse joint)
                                     (and (= limb :stance-leg) (zero? i))
                                     (- (get-in me [:components :torso :angle]))
                                     ;; others in parent coordinates
                                     :else
                                     (get-in me [:joints cmp-k :joint-angle]))
                           target-ang (+ (* dir theta)
                                         ;; balance feedback to swing hip
                                         (if (and (= limb :swing-leg) (zero? i))
                                           adj
                                           0))]]
                 [cmp-k
                  (if leg?
                    (turn-towards target-ang curr-ang ang-vel (:leg-speed params))
                    (turn-towards target-ang curr-ang ang-vel (:arm-speed params)
                                  (:arm-k-d params) (:arm-k-v params)))])
               (into {})
               (hash-map :joint-motors)))))

        curr-op (:operator state 0)
        curr-elapsed (- (:time (:current state))
                        (:operator-start state 0))
        [next-op actions elapsed] (loop [op curr-op
                                         elapsed curr-elapsed]
                                    (if-let [actions* (symmetric-gait walk op elapsed)]
                                      (let [actions (if balance?
                                                      actions*
                                                      (merge actions* react-arms))]
                                        [op actions elapsed])
                                      (recur (mod (inc op) (* 2 (count walk)))
                                             0.0)))
        ]
    (print (str next-op " "))
    (flush)
    (assoc state
      :actions actions
      :operator next-op
      :operator-start (- (:time (:current state)) elapsed))))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
