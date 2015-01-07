(ns org.nfrac.hatto.arena-simple
  (:require [cljbox2d.core :refer :all]))

(defn build!
  [world]
  (let [ground (body! world {:type :static}
                      {:shape (box 15 20 [0 -20])})]
    {:ground ground}))

