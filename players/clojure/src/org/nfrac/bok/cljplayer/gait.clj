(ns org.nfrac.bok.cljplayer.gait
  (:require [org.nfrac.bok.cljplayer.util :as util
             :refer [abs x-val turn-towards HALF_PI]]))

(def leg-cmps {:a [:leg-a1 :leg-a2 :leg-a3]
               :b [:leg-b1 :leg-b2 :leg-b3]})
(def arm-cmps {:a [:arm-a1 :arm-a2]
               :b [:arm-b1 :arm-b2]})

(defn x-offset
  [component]
  (x-val (:position (first (:points component)))))

(defn grounded?
  [cmp ground-entities]
  (some ground-entities
        (map :entity (:contacts cmp))))

(defn symmetric-gait
  "Performs a symmetric gait such as walking.
   Currently assumes humanoid creature. Adapted from Yin et al (2007)
   SIMBICON. This is a finite state machine. Returns an action map, or
   nil to reliquish control from the current op.

   With 2 stages:
   * op 0 ==> stage 0, stance a, swing b
   * op 1 ==> stage 1, stance a, swing b
   * op 2 ==> stage 0, stance b, swing a
   * op 3 ==> stage 1, stance b, swing a"
  [me dir stages current-op time-in-op ground-entities]
  (let [params (get stages (mod current-op (count stages)))
        stance (if (< current-op (count stages)) :a :b)
        swing (case stance :a :b, :b :a)
        swing-leg (mapv #(get-in me [:components %])
                        (get leg-cmps swing))
        ;; balance feedback to swing hip
        com (:center-of-mass me)
        comv (:velocity me)
        d (- (x-val com) (x-offset (second swing-leg)))
        adj (+ (* (:c-d params) d)
               (* (:c-v params) (x-val comv)))]
    (when-not (if (= :foot-contact (:dt params))
                (or (grounded? (second swing-leg) ground-entities)
                    (grounded? (last swing-leg) ground-entities)
                    ;; swing foot already behind
                    (neg? (* dir (:angle (first swing-leg)))))
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

(def humanoid-walk
  [{:dt 0.3
    :c-d 0.0
    :c-v 0.2
    ;;           \|/ this is torso angle (hip joint conversely)
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
    ;;           \|/ this is torso angle (hip joint conversely)
    :stance-leg [0.05 -0.1 (+ HALF_PI 0.2)]
    :swing-leg [-0.05 -0.1 (+ HALF_PI 0.0)]
    :stance-arm [-0.3 0.4]
    :swing-arm [0.3 0.4]
    :leg-speed 8
    :arm-speed 2
    :arm-k-d 40
    :arm-k-v 4
    }])
