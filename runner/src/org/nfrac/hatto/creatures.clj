(ns org.nfrac.hatto.creatures
  (:require [org.nfrac.hatto.core :refer [->BodyPois map->Entity]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [v-add]]))

(defmulti build
  (fn [type world _ _]
    type))

(defn revo-joint!
  [body-a body-b world-anchor]
  (joint! {:type :revolute
           :body-a body-a
           :body-b body-b
           :world-anchor world-anchor
           :max-motor-torque 1000}))

(defmethod build :nin
  [type world position group-index]
  (let [head (body! world {:position position}
                    {:shape (circle 0.5)
                     :density 10
                     :group-index group-index})
        tri-pts [[0.0 0.3]
                 [0.9 -0.4]
                 [-0.7 -0.4]]
        ;; first vertex internal so not a Point Of Interest
        tri-pois (drop 1 tri-pts)
        tri-fx {:shape (polygon tri-pts)
                :density 10
                :group-index group-index}
        limb-a (body! world {:position position} tri-fx)
        limb-b (body! world {:position position} tri-fx)
        rj-a (revo-joint! limb-a head position)
        rj-b (revo-joint! limb-b head position)]
    (map->Entity
     {:entity-type :creature
      :creature-type type
      :objects {:head (->BodyPois head [[0 0]])
                :limb-a (->BodyPois limb-a tri-pois)
                :limb-b (->BodyPois limb-b tri-pois)}
      :joints {:limb-a-rj rj-a
               :limb-b-rj rj-b}})))

(defmethod build :legge
  [type world position group-index]
  (let [head (body! world {:position position}
                    {:shape (circle 0.5)
                     :density 10
                     :group-index group-index})
        len 1.0
        limb-pois [[0.0 len]]
        thigh-fx {:shape (rod [0 0] 0 len 0.1)
                  :density 10
                  :group-index group-index}
        limb-a1 (body! world {:position position} thigh-fx)
        limb-b1 (body! world {:position position} thigh-fx)
        rj-a1 (revo-joint! limb-a1 head position)
        rj-b1 (revo-joint! limb-b1 head position)
        calf-fx thigh-fx
        calf-pos (v-add position [len 0.0])
        limb-a2 (body! world {:position calf-pos} calf-fx)
        limb-b2 (body! world {:position calf-pos} calf-fx)
        rj-a2 (revo-joint! limb-a1 limb-a2 calf-pos)
        rj-b2 (revo-joint! limb-b1 limb-b2 calf-pos)]
    (map->Entity
     {:entity-type :creature
      :creature-type type
      :objects {:head (->BodyPois head [[0 0]])
                :limb-a1 (->BodyPois limb-a1 limb-pois)
                :limb-b1 (->BodyPois limb-b1 limb-pois)
                :limb-a2 (->BodyPois limb-a2 limb-pois)
                :limb-b2 (->BodyPois limb-b2 limb-pois)}
      :joints {:limb-a1-rj rj-a1
               :limb-b1-rj rj-b1
               :limb-a2-rj rj-a2
               :limb-b2-rj rj-b2}})))

(defmethod build :hatto
  [type world position group-index]
  (let [head (body! world {:position position}
                    {:shape (circle 0.5)
                     :density 10
                     :group-index group-index})
        thigh-len 1.0
        thigh-pois [[0.0 thigh-len]]
        thigh-fx {:shape (rod [0 0] 0 thigh-len 0.1)
                  :density 10
                  :group-index group-index}
        limb-a (body! world {:position position} thigh-fx)
        limb-b (body! world {:position position} thigh-fx)
        rj-a (revo-joint! limb-a head position)
        rj-b (revo-joint! limb-b head position)
        calf-pos (v-add position [thigh-len 0.0])
        calf-len (* thigh-len 0.67)
        calf-pois [[0.0 calf-len]]
        calf-fx (assoc thigh-fx :shape (rod [0 0] 0 calf-len 0.1))
        limb-aa (body! world {:position calf-pos} calf-fx)
        limb-ab (body! world {:position calf-pos} calf-fx)
        limb-ba (body! world {:position calf-pos} calf-fx)
        rj-aa (revo-joint! limb-aa limb-a calf-pos)
        rj-ab (revo-joint! limb-ab limb-a calf-pos)
        rj-ba (revo-joint! limb-ba limb-b calf-pos)
        toe-pos (v-add calf-pos [calf-len 0.0])
        toe-len (* calf-len 0.67)
        toe-pois [[0.0 toe-len]]
        toe-fx (assoc calf-fx :shape (rod [0 0] 0 toe-len 0.1))
        limb-aaa (body! world {:position toe-pos} toe-fx)
        limb-aab (body! world {:position toe-pos} toe-fx)
        limb-aba (body! world {:position toe-pos} toe-fx)
        limb-abb (body! world {:position toe-pos} toe-fx)
        limb-baa (body! world {:position toe-pos} toe-fx)
        rj-aaa (revo-joint! limb-aaa limb-aa toe-pos)
        rj-aab (revo-joint! limb-aab limb-aa toe-pos)
        rj-aba (revo-joint! limb-aba limb-ab toe-pos)
        rj-abb (revo-joint! limb-abb limb-ab toe-pos)
        rj-baa (revo-joint! limb-baa limb-ba toe-pos)]
    (map->Entity
     {:entity-type :creature
      :creature-type type
      :objects {:head (->BodyPois head [[0 0]])
                :limb-a (->BodyPois limb-a thigh-pois)
                :limb-b (->BodyPois limb-a thigh-pois)
                :limb-aa (->BodyPois limb-aa calf-pois)
                :limb-ab (->BodyPois limb-ab calf-pois)
                :limb-ba (->BodyPois limb-ba calf-pois)
                :limb-aaa (->BodyPois limb-aaa toe-pois)
                :limb-aab (->BodyPois limb-aab toe-pois)
                :limb-aba (->BodyPois limb-aba toe-pois)
                :limb-abb (->BodyPois limb-abb toe-pois)
                :limb-baa (->BodyPois limb-baa toe-pois)}
      :joints {:limb-a-rj rj-a
               :limb-b-rj rj-a
               :limb-aa-rj rj-aa
               :limb-ab-rj rj-ab
               :limb-ba-rj rj-ba
               :limb-aaa-rj rj-aaa
               :limb-aab-rj rj-aab
               :limb-aba-rj rj-aba
               :limb-abb-rj rj-abb
               :limb-baa-rj rj-baa}})))

