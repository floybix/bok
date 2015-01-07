(ns org.nfrac.hatto.players
  (:require [org.nfrac.hatto.core :refer [->BodyPois]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [v-add]]))

(defn nin
  [world position group-index]
  (let [head (body! world {:position position}
                    {:shape (circle 0.5)
                     :density 10
                     :group-index group-index})
        tri-pts [[0.0 0.3]
                 [0.9 -0.4]
                 [-0.7 -0.4]]
        tri-fx {:shape (polygon tri-pts)
                :density 10
                :group-index group-index}
        ;; first vertex internal so not a Point Of Interest
        tri-pois (drop 1 tri-pts)
        limb-a (body! world {:position position}
                      tri-fx)
        limb-b (body! world {:position position}
                      tri-fx)
        rj-a (joint! {:type :revolute
                      :body-a limb-a
                      :body-b head
                      :world-anchor position
                      :max-motor-torque 1000})
        rj-b (joint! {:type :revolute
                      :body-a limb-b
                      :body-b head
                      :world-anchor position
                      :max-motor-torque 1000})]
    {:entity-type :nin
     :limbs {:head (->BodyPois head [[0 0]])
             :limb-a (->BodyPois limb-a tri-pois)
             :limb-b (->BodyPois limb-b tri-pois)}
     :joints {:limb-a-rj rj-a
              :limb-b-rj rj-b}}))

(defn legge
  [world position group-index]
  (let [head (body! world {:position position}
                    {:shape (circle 0.5)
                     :density 10
                     :group-index group-index})
        len 1.0
        limb-pois [[0.0 0.0]
                   [0.0 len]]
        thigh-fx {:shape (rod [0 0] 0 len 0.1)
                  :density 10
                  :group-index group-index}
        limb-a1 (body! world {:position position}
                       thigh-fx)
        limb-b1 (body! world {:position position}
                       thigh-fx)
        rj-a1 (joint! {:type :revolute
                       :body-a limb-a1
                       :body-b head
                       :world-anchor position
                       :max-motor-torque 1000})
        rj-b1 (joint! {:type :revolute
                       :body-a limb-b1
                       :body-b head
                       :world-anchor position
                       :max-motor-torque 1000})
        calf-fx thigh-fx
        calf-pos (v-add position [len 0.0])
        limb-a2 (body! world {:position calf-pos}
                       calf-fx)
        limb-b2 (body! world {:position calf-pos}
                       calf-fx)
        rj-a2 (joint! {:type :revolute
                       :body-a limb-a2
                       :body-b limb-a1
                       :world-anchor calf-pos
                       :max-motor-torque 1000})
        rj-b2 (joint! {:type :revolute
                       :body-a limb-b2
                       :body-b limb-b1
                       :world-anchor calf-pos
                       :max-motor-torque 1000})]
    {:entity-type :legge
     :limbs {:head (->BodyPois head [[0 0]])
             :limb-a1 (->BodyPois limb-a1 limb-pois)
             :limb-b1 (->BodyPois limb-b1 limb-pois)
             :limb-a2 (->BodyPois limb-a2 limb-pois)
             :limb-b2 (->BodyPois limb-b2 limb-pois)}
     :joints {:limb-a1-rj rj-a1
              :limb-b1-rj rj-b1
              :limb-a2-rj rj-a2
              :limb-b2-rj rj-b2}}))

(defn hatto
  [world position group-index]
  )
