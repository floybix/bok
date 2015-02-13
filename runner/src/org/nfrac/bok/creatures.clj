(ns org.nfrac.bok.creatures
  (:require [org.nfrac.bok.entities :refer [set-pois entity]]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [v-add]]))

(defmulti build
  "Constructs a creature of type `type` in the JBox2D world, returning
   it as an entity map. It will be standing at the given ground
   position. Each player should be given a unique negative integer
   group-index to filter self-collisions."
  (fn [type world position group-index]
    type))

(def DOWN (* -0.5 Math/PI))

(defn limb
  "Builds a limb of consecutive segments based on the `lengths` sequence.
   Each segment is by default attached with a revolute joint, and the
   first is likewise attached to `host` body. If `:joints?` is given
   any false elements are instead created as weld joints. Returns
   keys :components and :joints, each a map with keywords made from
   `prefix` and a number. Joints are keyed by their outer component
   name."
  [world host position fixture-spec
   & {:keys [lengths widths joints? prefix]
      :or {lengths [0.75 0.75]
           widths (repeat 0.1)
           joints? (repeat true)
           prefix "seg-"}}]
  (let [segs (loop [i 0
                    pos position
                    segs []]
               (if (>= i (count lengths))
                 segs
                 (let [len (nth lengths i)
                       width (nth widths i)
                       joint? (nth joints? i)
                       seg (-> (body! world {:position pos}
                                      (assoc fixture-spec
                                        :shape (rod [0 0] DOWN len width)))
                               (set-pois [[0.0 (- len)]]))
                       prev (or (:component (peek segs))
                                host)
                       jt (joint! {:type (if joint? :revolute :weld)
                                   :body-a prev
                                   :body-b seg
                                   :world-anchor pos})]
                   (recur (inc i)
                          (v-add pos [0.0 (- (- len (/ width 2)))])
                          (conj segs {:key (keyword (str prefix (inc i)))
                                      :component seg
                                      :joint (when joint? jt)})))))]
    {:components (into {} (map (juxt :key :component) segs))
     :joints (into {} (map (juxt :key :joint) (filter :joint segs)))}))

(defmethod build :bipoid
  [type world position group-index]
  (let [head-pos (v-add position [0 2.0])
        head (-> (body! world {:position head-pos}
                        {:shape (circle 0.25)
                         :density 10
                         :friction 1
                         :restitution 0
                         :group-index group-index})
                 (set-pois [[0 0]]))
        limb-spec {:density 20
                   :friction 1
                   :restitution 0
                   :group-index group-index}
        limbs (merge-with
               merge ;; merge nested maps
               (limb world head head-pos limb-spec
                     :lengths [1.0 1.0] :prefix "leg-a" :joints? [false true])
               (limb world head head-pos limb-spec
                     :lengths [1.0 1.0] :prefix "leg-b"))]
    (entity (assoc (:components limbs)
              :head head)
            :joints (:joints limbs)
            :entity-type :creature
            :creature-type type
            :group-index group-index)))

(defmethod build :humanoid
  [type world position group-index]
  (let [head-pos (v-add position [0 2.9])
        head (-> (body! world {:position head-pos}
                        {:shape (circle 0.25)
                         :density 5
                         :friction 1
                         :restitution 0
                         :group-index group-index})
                 (set-pois [[0 0]]))
        torso-pos (v-add head-pos [0 -0.75])
        torso-pts [[0.00 0.50]
                   [-0.25 0.25]
                   [-0.25 -0.25]
                   [0.00 -0.50]
                   [0.25 -0.25]
                   [0.25 0.25]]
        torso-pois [[-0.25 0.0]
                    [0.25 0.0]]
        torso (-> (body! world {:position torso-pos}
                         {:shape (polygon torso-pts)
                          :density 5
                          :friction 1
                          :restitution 0
                          :group-index group-index})
                  (set-pois torso-pois))
        wj (joint! {:type :weld :body-a head :body-b torso
                    :world-anchor head-pos})
        limb-spec {:density 20
                   :friction 1
                   :restitution 0
                   :group-index group-index}
        arm-pos (v-add torso-pos [0.0 0.40])
        leg-pos (v-add torso-pos [0.0 -0.40])
        limbs (merge-with
               merge ;; merge nested maps
               (limb world torso arm-pos (assoc limb-spec :density 5)
                     :lengths [0.75 0.75] :prefix "arm-a")
               (limb world torso arm-pos (assoc limb-spec :density 5)
                     :lengths [0.75 0.75] :prefix "arm-b")
               (limb world torso leg-pos limb-spec
                     :lengths [0.75 0.75 0.25] :prefix "leg-a")
               (limb world torso leg-pos limb-spec
                     :lengths [0.75 0.75 0.25] :prefix "leg-b"))]
    (entity (assoc (:components limbs)
              :head head
              :torso torso)
            :joints (:joints limbs)
            :entity-type :creature
            :creature-type type
            :group-index group-index)))

(defmethod build :wormoid
  [type world position group-index]
  (let [head-pos (v-add position [0 3.75])
        head (-> (body! world {:position head-pos}
                        {:shape (circle 0.25)
                         :density 10
                         :friction 1
                         :restitution 0
                         :group-index group-index})
                 (set-pois [[0 0]]))
        limb-spec {:density 20
                   :friction 1
                   :restitution 0
                   :group-index group-index}
        segs (limb world head head-pos limb-spec
                   :lengths (repeat 5 0.75) :prefix "seg-"
                   ;; first segment is fixed to the head (not revolute)
                   :joints? (cons false (repeat true)))]
    (entity (assoc (:components segs)
              :head head)
            :joints (:joints segs)
            :entity-type :creature
            :creature-type type
            :group-index group-index)))
