(ns org.nfrac.bok.creatures
  (:require [org.nfrac.bok.entities :refer [set-pois entity]]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [v-add]]))

(defmulti build
  (fn [type world _ _]
    type))

(defn revo-joint!
  [body-a body-b world-anchor & {:as more}]
  (joint! (into {:type :revolute
                 :body-a body-a
                 :body-b body-b
                 :world-anchor world-anchor
                 :max-motor-torque 100}
                more)))

(defn limb
  "Builds a limb of `n-segments`, each of length `seg-len` metres.
   Each segment is attached with a revolute joint, and the first is
   likewise attached to `host` body. Returns keys :components
   and :joints, each a map with keywords constructed using `prefix`."
  [world host position seg-len fixture-spec
   & {:keys [width n-segments prefix]
      :or {width 0.1, n-segments 2, prefix "seg-"}}]
  (let [pois [[seg-len 0.0]]
        seg-fx (assoc fixture-spec
                 :shape (rod [0 0] 0 seg-len width))
        segs (reduce (fn [segs i]
                       (let [pos (v-add position [(* i seg-len) 0.0])
                             seg (-> (body! world {:position pos}
                                            seg-fx)
                                     (set-pois pois))
                             prev (or (:component (peek segs))
                                      host)
                             rj (revo-joint! prev seg pos)]
                         (conj segs {:key (keyword (str prefix (inc i)))
                                     :joint-key (keyword (str prefix (inc i) "-rj"))
                                     :component seg
                                     :joint rj})))
                     []
                     (range n-segments))]
    {:components (into {} (map (juxt :key :component) segs))
     :joints (into {} (map (juxt :joint-key :joint) segs))}))

(defmethod build :legsoid
  [type world position group-index]
  (let [head (-> (body! world {:position position
                               :fixed-rotation true}
                        {:shape (circle 0.5)
                         :density 5
                         :friction 0.5
                         :group-index group-index})
                 (set-pois [[0 0]]))
        limb-spec {:density 10
                   :friction 0.5
                   :group-index group-index}
        limbs (merge-with
               merge ;; merge nested maps
               (limb world head position 1.0 limb-spec
                     :n-segments 2 :prefix "limb-a")
               (limb world head position 1.0 limb-spec
                     :n-segments 2 :prefix "limb-b"))]
    (entity (assoc (:components limbs)
              :head head)
            :joints (:joints limbs)
            :entity-type :creature
            :creature-type type
            :group-index group-index)))

(defmethod build :humanoid
  [type world position group-index]
  (let [head-pos (v-add position [0 0.75])
        head (-> (body! world {:position head-pos}
                        {:shape (circle 0.25)
                         :density 5
                         :friction 0.5
                         :group-index group-index})
                 (set-pois [[0 0]]))
        torso-pos position
        torso-pts [[0.00 0.50]
                   [-0.25 0.25]
                   [-0.25 -0.25]
                   [0.25 -0.25]
                   [0.25 0.25]]
        torso-pois [[-0.25 0.0]
                    [0.25 0.0]]
        torso (-> (body! world {:position torso-pos}
                         {:shape (polygon torso-pts)
                          :density 5
                          :friction 0.5
                          :group-index group-index})
                  (set-pois torso-pois))
        wj (joint! {:type :weld :body-a head :body-b torso
                    :world-anchor head-pos})
        pelvis-pos (v-add torso-pos [0.0 -0.40])
        pelvis (-> (body! world {:position pelvis-pos
                                 :fixed-rotation true}
                          {:shape (circle 0.25)
                           :density 5
                           :friction 0.5
                           :group-index group-index})
                   (set-pois [[0 0]]))
        pelvis-rj (revo-joint! pelvis torso pelvis-pos
                               ;:enable-limit true
                               :lower-angle (- (/ Math/PI 2))
                               :upper-angle (/ Math/PI 2))
        limb-spec {:density 10
                   :friction 0.5
                   :group-index group-index}
        arm-pos (v-add torso-pos [0.0 0.40])
        leg-pos pelvis-pos
        limbs (merge-with
               merge ;; merge nested maps
               (limb world torso arm-pos 0.75 limb-spec
                     :n-segments 2 :prefix "arm-a")
               (limb world torso arm-pos 0.75 limb-spec
                     :n-segments 2 :prefix "arm-b")
               (limb world torso leg-pos 0.75 limb-spec
                     :n-segments 2 :prefix "leg-a")
               (limb world torso leg-pos 0.75 limb-spec
                     :n-segments 2 :prefix "leg-b"))]
    (entity (assoc (:components limbs)
              :head head
              :torso torso
              :pelvis pelvis)
            :joints (assoc (:joints limbs)
                      :pelvis-rj pelvis-rj)
            :entity-type :creature
            :creature-type type
            :group-index group-index)))

(defmethod build :wormoid
  [type world position group-index]
  (let [head-pos (v-add position [-2.5 0])
        head (-> (body! world {:position head-pos}
                        {:shape (circle 0.25)
                         :density 5
                         :friction 0.5
                         :group-index group-index})
                 (set-pois [[0 0]]))
        limb-spec {:density 10
                   :friction 0.5
                   :group-index group-index}
        segs (limb world head head-pos 1.0 limb-spec
                   :n-segments 5 :prefix "seg-")]
    (entity (assoc (:components segs)
              :head head)
            :joints (:joints segs)
            :entity-type :creature
            :creature-type type
            :group-index group-index)))
