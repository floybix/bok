(ns org.nfrac.hatto.creatures
  (:require [org.nfrac.hatto.entities :refer [with-pois map->Entity]]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [v-add]]))

(defmulti build
  (fn [type world _ _]
    type))

(defn revo-joint!
  [body-a body-b world-anchor]
  (joint! {:type :revolute
           :body-a body-a
           :body-b body-b
           :world-anchor world-anchor
           :max-motor-torque 100}))

(defn limb
  [world host position seg-len fixture-spec
   & {:keys [width n-segments prefix]
      :or {width 0.1, n-segments 2, prefix "seg-"}}]
  (let [pois [[seg-len 0.0]]
        seg-fx (assoc fixture-spec
                 :shape (rod [0 0] 0 seg-len width))
        segs (reduce (fn [segs i]
                       (let [pos (v-add position [(* i seg-len) 0.0])
                             seg (body! world {:position pos}
                                        seg-fx)
                             prev (or (:component (peek segs))
                                      host)
                             rj (revo-joint! prev seg pos)]
                         (conj segs {:key (keyword (str prefix (inc i)))
                                     :joint-key (keyword (str prefix (inc i) "-rj"))
                                     :component (with-pois seg pois)
                                     :joint rj})))
                     []
                     (range n-segments))]
    {:components (into {} (map (juxt :key :component) segs))
     :joints (into {} (map (juxt :joint-key :joint) segs))}))

(defmethod build :legsoid
  [type world position group-index]
  (let [head (body! world {:position position
                           :fixed-rotation true}
                    {:shape (circle 0.5)
                     :density 5
                     :friction 0.5
                     :group-index group-index})
        limb-spec {:density 10
                   :friction 0.5
                   :group-index group-index}
        limbs (merge-with
               merge
               (limb world head position 1.0 limb-spec
                     :n-segments 2 :prefix "limb-a")
               (limb world head position 1.0 limb-spec
                     :n-segments 2 :prefix "limb-b"))]
    (map->Entity
     {:entity-type :creature
      :creature-type type
      :components (assoc (:components limbs)
                    :head (with-pois head [[0 0]]))
      :joints (:joints limbs)})))

(defmethod build :humanoid
  [type world position group-index]
  (let [head-pos (v-add position [0 0.75])
        head (body! world {:position head-pos}
                    {:shape (circle 0.25)
                     :density 5
                     :friction 0.5
                     :group-index group-index})
        torso-pos position
        torso-pts [[0.00 0.50]
                   [-0.25 0.25]
                   [-0.25 -0.25]
                   [0.00 -0.50]
                   [0.25 -0.25]
                   [0.25 0.25]]
        torso-pois [[-0.25 0.0]
                    [0.25 0.0]]
        torso (body! world {:position torso-pos}
                     {:shape (polygon torso-pts)
                      :density 5
                      :friction 0.5
                      :group-index group-index})
        wj (joint! {:type :weld :body-a head :body-b torso
                    :world-anchor head-pos})
        limb-spec {:density 10
                   :friction 0.5
                   :group-index group-index}
        arm-pos (v-add torso-pos [0.0 0.40])
        leg-pos (v-add torso-pos [0.0 -0.40])
        limbs (merge-with
               merge
               (limb world torso arm-pos 0.75 limb-spec
                     :n-segments 2 :prefix "arm-a")
               (limb world torso arm-pos 0.75 limb-spec
                     :n-segments 2 :prefix "arm-b")
               (limb world torso leg-pos 0.75 limb-spec
                     :n-segments 2 :prefix "leg-a")
               (limb world torso leg-pos 0.75 limb-spec
                     :n-segments 2 :prefix "leg-b"))]
    (map->Entity
     {:entity-type :creature
      :creature-type type
      :components (assoc (:components limbs)
                    :head (with-pois head [[0 0]])
                    :torso (with-pois torso torso-pois))
      :joints (:joints limbs)})))

(defmethod build :wormoid
  [type world position group-index]
  (let [head-pos (v-add position [-2.5 0])
        head (body! world {:position head-pos}
                    {:shape (circle 0.25)
                     :density 5
                     :friction 0.5
                     :group-index group-index})
        limb-spec {:density 10
                   :friction 0.5
                   :group-index group-index}
        segs (limb world head head-pos 1.0 limb-spec
                   :n-segments 5 :prefix "seg-")]
    (map->Entity
     {:entity-type :creature
      :creature-type type
      :components (assoc (:components segs)
                    :head (with-pois head [[0 0]]))
      :joints (:joints segs)})))
