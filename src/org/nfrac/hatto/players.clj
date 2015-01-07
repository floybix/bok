(ns org.nfrac.hatto.players
  (:require [org.nfrac.hatto.core :refer [->BodyPois]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]))

(defn nin
  [world position group-index]
  (let [head (body! world {:position position}
                    {:shape (circle 0.5)
                     :density 10
                     :group-index group-index})
        tri-fx {:shape (polygon [[0.0 0.3]
                                 [0.9 -0.4]
                                 [-0.7 -0.4]])
                :density 10
                :group-index group-index}
        tri-pois [[0.9 -0.4]
                  [-0.9 -0.4]]
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
