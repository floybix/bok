(ns org.nfrac.hatto.arenas
  (:require [org.nfrac.hatto.data :refer [->BodyPois map->Entity]]
            [cljbox2d.core :refer :all]))

(defmulti build
  (fn [type world]
    type))

(defmethod build :simple
  [type world]
  (let [ground (body! world {:type :static}
                      {:shape (box 15 20 [0 -20])
                       :friction 1.0})
        pois [[-15 0] [15 0]]]
    (map->Entity
     {:entity-type :arena
      :arena-type type
      :components {:ground (->BodyPois ground pois)}})))

(defmethod build :sandbox
  [type world]
  (let [ground (body! world {:type :static}
                      {:shape (box 15 20 [0 -20])
                       :friction 1.0}
                      {:shape (edge [-15 0] [-15 15])}
                      {:shape (edge [15 0] [15 15])})
        pois [[-15 0] [15 0]]]
    (map->Entity
     {:entity-type :arena
      :arena-type type
      :components {:ground (->BodyPois ground pois)}})))
