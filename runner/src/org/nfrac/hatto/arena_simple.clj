(ns org.nfrac.hatto.arena-simple
  (:require [org.nfrac.hatto.core :refer [->BodyPois map->Entity]]
            [cljbox2d.core :refer :all]))

(defn build!
  [world]
  (let [ground (body! world {:type :static}
                      {:shape (box 15 20 [0 -20])})
        pois [[-15 0] [15 0]]]
    (map->Entity
     {:entity-type :arena
      :objects {:ground (->BodyPois ground pois)}})))
