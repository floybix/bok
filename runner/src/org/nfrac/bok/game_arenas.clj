(ns org.nfrac.bok.game-arenas
  (:require [org.nfrac.bok.games :as games :refer [build*]]
            [org.nfrac.bok.entities :as ent :refer [simple-entity]]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d
             :refer [v-add v-sub polar-xy v-interp v-scale]]))

;; =============================================================================
;; ## Sandbox.
;;
;; Just a flat ground and side walls. For testing.

(defmethod build* :sandbox
  [_ players _]
  (let [world (new-world)
        ground (simple-entity
                [[-15 0] [15 0]]
                (body! world {:type :static}
                       {:shape (box 15 20 [0 -20])
                        :friction 1}))
        left-wall (simple-entity
                   [[0 0] [0 15]]
                   (body! world {:type :static
                                 :position [-15 0]}
                          {:shape (edge [0 0] [0 15])}))
        right-wall (simple-entity
                    [[0 0] [0 15]]
                    (body! world {:type :static
                                  :position [15 0]}
                           {:shape (edge [0 0] [0 15])}))
        entities {:ground ground
                  :left-wall left-wall
                  :right-wall right-wall}
        starting-pts [[-10 0] [10 0] [0 0] [-5 0] [5 0]]]
    (->
     (games/sumo-game world entities players starting-pts -2.0)
     (assoc :game-version [0 0 1]
            :game-over-secs nil
            :camera {:width 40 :height 20 :center [0 5]}))))

;; =============================================================================
;; ## Sumo
;;
;; Just a flat ground/platform.

(defmethod build* :sumo
  [_ players _]
  (let [world (new-world)
        ground (simple-entity
                [[-15 0] [15 0]]
                (body! world {:type :static}
                       {:shape (box 15 20 [0 -20])
                        :friction 1}))
        entities {:ground ground}
        starting-pts [[-10 0] [10 0] [0 0] [-5 0] [5 0]]]
    (->
     (games/sumo-game world entities players starting-pts -2.0)
     (assoc :game-version [0 0 1]
            :camera {:width 40 :height 20 :center [0 5]}))))

;; =============================================================================
;; ## Altitude
;;
;; Arena has flat ground with a gulley in the middle, large and small
;; blocks, two platforms, a central swing (on rope joints) and a
;; stalactite with hand-holds.

(defmethod build* :altitude-one
  [_ players _]
  (let [world (new-world)
        ground-pts [[-16 0] [-4 0] [0 -2] [4 0] [16 0]]
        ground (simple-entity
                ground-pts
                (body! world {:type :static}
                       {:shape (edge-chain ground-pts)
                        :friction 1}))
        ceil-y 16
        ceil-hole-x 3
        ceiling (simple-entity
                 [[-16 0] [(- ceil-hole-x) 0]
                  [16 0] [ceil-hole-x 0]]
                 (body! world {:type :static
                               :position [0 ceil-y]}
                        {:shape (edge [-16 0] [(- ceil-hole-x) 0])}
                        {:shape (edge [ceil-hole-x 0] [16 0])}))
        stal-depth 6
        stal-yend 10
        stal-xoff 1
        stal-ladd-pts (for [sign [-1 1]
                            frac (range 0.1 0.9 0.2)]
                        (let [base-pt (v-interp [0 (- stal-depth)]
                                                [(* sign stal-xoff) 0]
                                                frac)
                              out-pt (v-add base-pt [(* sign 0.5) 0.1])]
                          [base-pt out-pt]))
        stalactite (simple-entity
                    (concat [[stal-xoff 0]
                             [(- stal-xoff) 0]
                             [0 (- stal-depth)]]
                            (map second stal-ladd-pts))
                    (apply body! world {:type :static
                                        :position [0 ceil-y]}
                           {:shape (polygon [[stal-xoff 0]
                                             [(- stal-xoff) 0]
                                             [0 (- stal-depth)]])
                            :friction 1}
                           (for [[base-pt out-pt] stal-ladd-pts]
                             {:shape (edge base-pt out-pt)
                              :friction 1})))
        rope-length 10
        swing-pois [[-2 0] [2 0]]
        swing (simple-entity
               swing-pois
               (body! world {:position [0 (- ceil-y rope-length)]}
                      {:shape (box 2 0.1)
                       :friction 1})
               :entity-type :movable)
        ropes (doall
               (for [[x y] swing-pois]
                 (joint! {:type :rope
                          :body-a (ent/entity-body ceiling)
                          :body-b (ent/entity-body swing)
                          :anchor-a [(* x 2) 0]
                          :anchor-b [x y]
                          :max-length rope-length})))
        block-fx {:shape (box 2 1.5)
                  :density 2
                  :friction 1}
        block-pois [[-2 1.5]
                    [2 1.5]]
        blocks (for [[sign kw] [[-1 :block-left] [1 :block-right]]]
                 [kw (simple-entity
                      block-pois
                      (body! world {:position [(* sign 13) 1.5]}
                             block-fx)
                      :entity-type :movable)])
        mini-fx {:shape (box 0.5 0.5)
                 :density 2
                 :friction 1}
        mini-pois [[-0.5 0.5]
                   [0.5 0.5]
                   [0.5 -0.5]
                   [-0.5 -0.5]]
        minis (for [[sign kw] [[-1 :mini-left] [1 :mini-right]]]
                [kw (simple-entity
                     mini-pois
                     (body! world {:position [(* sign 4.5) 0.5]}
                            mini-fx)
                     :entity-type :movable)])
        plat-fx {:shape (edge [-2 0] [2 0])
                 :friction 1}
        plat-pois [[-2 0] [2 0]]
        plats (for [[sign kw] [[-1 :plat-left] [1 :plat-right]]]
                [kw (simple-entity
                     plat-pois
                     (body! world {:type :static
                                   :position [(* sign 8) 6]}
                            plat-fx))])
        entities (into {:ground ground
                        :ceiling ceiling
                        :stalactite stalactite
                        :swing swing}
                       (concat blocks minis plats))
        starting-pts (map vector [-10 10 0 -3 3] (repeat 0))]
    (->
     (games/altitude-game world entities players starting-pts)
     (assoc :game-version [0 0 1]
            :camera {:width 40 :height 20 :center [0 7]}))))

;; =============================================================================
;; ## Energy Race
;;
;; Arena is a x^4 curve with a sine wave mixed in for foot-holds. Also
;; two high platforms.

(defmethod build* :energy-race-one
  [_ players _]
  (let [world (new-world)
        surface-fn (fn [x]
                     (+ (* (Math/pow x 4)
                           (/ (Math/pow 20 4))
                           20.0)
                        (* (Math/cos (* x 5))
                           (/ (* x x) (* 20 20))
                           1.2)))
        surface (for [x (range -21 21.01 0.2)]
                  [x (surface-fn x)])
        ;; split the surface into 3 smaller bodies, or too big for Box2d?
        cliff-n (long (* 0.25 (dec (count surface))))
        ground-n (- (count surface) (* 2 cliff-n))
        [left-pts ground-pts right-pts] [(take (inc cliff-n) surface)
                                         (take ground-n
                                               (drop cliff-n surface))
                                         (take-last (inc cliff-n) surface)]
        ground (simple-entity
                [(first ground-pts) (last ground-pts)]
                (body! world {:type :static}
                       {:shape (edge-chain ground-pts)
                        :friction 1}))
        left-cliff (simple-entity
                    [(first left-pts) (last left-pts)]
                    (body! world {:type :static}
                           {:shape (edge-chain left-pts)
                            :friction 1}))
        right-cliff (simple-entity
                     [(first right-pts) (last right-pts)]
                     (body! world {:type :static}
                            {:shape (edge-chain right-pts)
                             :friction 1}))
        plat-fx {:shape (edge [-5.5 0] [5.5 0])
                 :friction 1}
        plat-pois [[-5.5 0] [5.5 0]]
        plat-right (simple-entity
                    plat-pois
                    (body! world {:type :static
                                  :position [8 14]}
                           plat-fx))
        plat-left (simple-entity
                   plat-pois
                   (body! world {:type :static
                                 :position [-8 14]}
                          plat-fx))
        ;; place food by scanning down from above at regular intervals
        food-pts (for [sign [-1 1]
                       xa (concat [2 4 6]
                                  (range 10.4 20 1))
                       :let [x (* sign xa)]]
                   (let [[rc] (raycast world [x 15] [x 13] :closest)
                         pt (if rc (:point rc) [x (surface-fn x)])]
                     (v-add pt [(- sign) 1.5])))
        food-fx {:shape (polygon [[0 0.6] [-0.3 0] [0.3 0]])
                 :density 5 :restitution 0 :friction 1}
        ;; set food to be floating (gravity-scale 0);
        ;; we'll switch on gravity if it's touched later
        foods (map-indexed (fn [i pos]
                             (let [k (keyword (str "food-" i))]
                               [k (simple-entity
                                   [[0 0]]
                                   (body! world {:position pos
                                                 :gravity-scale 0.0
                                                 :user-data {:food-joules 200.0}}
                                          food-fx)
                                   :entity-type :food)]))
                           food-pts)
        entities (into {:ground ground
                        :left-cliff left-cliff
                        :right-cliff right-cliff
                        :plat-left plat-left
                        :plat-right plat-right}
                       foods)
        starting-pts (map vector [-8 8 0 -4 4] (repeat 1))]
    (->
     (games/energy-race-game world entities players starting-pts)
     (assoc :game-version [0 0 1]
       :camera {:width 40 :height 22 :center [0 10]}))))

;; =============================================================================
;; ## Hunt
;;
;; Arena has a flat ground with a central pedestal, slopes leading up
;; and towards the center from both left and right (meeting at a
;; central gap), a high left platform, and a ladder on the left wall.
;; Also two movable boulders.

(defmethod build* :hunt-one
  [_ players _]
  (let [world (new-world)
        east-x 15
        west-x -15
        ceil-y 14
        ground (simple-entity
                [[west-x 0] [east-x 0]]
                (body! world {:type :static}
                       {:shape (edge [west-x 0] [east-x 0])
                        :friction 1}))
        left-wall (simple-entity
                   [[0 0] [0 ceil-y]]
                   (body! world {:type :static
                                 :position [west-x 0]}
                          {:shape (edge [0 0] [0 ceil-y])}))
        right-wall (simple-entity
                    [[0 0] [0 ceil-y]]
                    (body! world {:type :static
                                  :position [east-x 0]}
                           {:shape (edge [0 0] [0 ceil-y])}))
        ceiling (simple-entity
                 [[west-x 0] [east-x 0]]
                 (body! world {:type :static
                               :position [0 ceil-y]}
                        {:shape (edge [west-x 0] [east-x 0])}))
        ;; -4m to left, 4.1m to right, 2.3m up
        pedestal-pts [[-4.0 0.0] [-2.5 0.2] [-2.0 0.5] [-2.1 1.1] [-1.9 1.8] [-1.5 2.0]
                      [4.0 2.3] [4.1 2.1] [3.5 1.7] [3.0 1.0] [3.1 0.7] [3.25 0.0]]
        pedestal (simple-entity
                  [[-2.0 0.5] [-1.5 2.0] [4.0 2.3] [3.25 0.0]]
                  (body! world {:type :static
                                :position [2 0]}
                         {:shape (edge-chain pedestal-pts)
                          :friction 1}))
        ;; 3m to left, 3.5m to right
        r-ramp-pts (into [[-3.0 0.0] [-2.0 1.0] [-1 0.9] [0.0 0.5]]
                         ;; steps continue ramp
                         (reductions v-add
                                     [0.0 1.0]
                                     (interpose [0.0 0.5]
                                                (repeat 7 [0.5 0.0]))))
        r-ramp (simple-entity
                (take-nth 2 r-ramp-pts)
                (body! world {:type :static
                              :position [(- east-x 3.5) 0]}
                       {:shape (edge-chain r-ramp-pts)
                        :friction 1}))
        ;; 3m to left, 3m to right. 1.5m down, 1m up
        steps-pts (reductions v-add
                              [3 -1.5]
                              (interpose [0.0 0.5]
                                         (repeat 6 [-1.0 0.0])))
        steps (simple-entity
               (concat (take-nth 2 steps-pts)
                       (take-last 1 steps-pts))
               (body! world {:type :static
                             :position [(- east-x 7) 7]}
                      {:shape (edge-chain steps-pts)
                       :friction 1}))
        ;; 1m to left, 1.5m to right
        l-ramp-pts [[-1.0 1.2] [0.5 0.3] [1.5 0.0]]
        l-ramp (simple-entity
                l-ramp-pts
                (body! world {:type :static
                              :position [(+ west-x 1) 0]}
                       {:shape (edge-chain l-ramp-pts)
                        :friction 1}))
        ;; 7m to left, 7m to right. 3m down, 2m up
        slope-pts [[-7 -3] [-6 -3] [-5 -2.5] [-4.2 -2.6] [-3 -2] [-2 -1.8]
                   [-1.6 -1.1] [-0.5 -0.9] [0 -1] [1 -0.7] [1.5 0.1] [2 0]
                   [2.1 0.5] [3 1] [5 1] [5.5 1.2] [6 2] [7 2]]
        slope (simple-entity
               (concat (take-nth 3 slope-pts)
                       (take-last 1 slope-pts))
               (body! world {:type :static
                             :position [-5 6]}
                      {:shape (edge-chain slope-pts)
                       :friction 1}))
        ;; 3m to left, 3m to right
        plat-fx {:shape (edge [-3 0] [3 0])
                 :friction 1}
        plat-pois [[-3 0] [3 0]]
        plat (simple-entity
              plat-pois
              (body! world {:type :static
                            :position [(+ west-x 6) 10]}
                     plat-fx))
        ;; 5m up, 0.5m right
        ladder-pois (for [y (range 0 5.01)]
                      [0.5 y])
        ladder (simple-entity
                ladder-pois
                (apply body! world {:type :static
                                    :position [(+ west-x 0.15) 5]}
                       ;; surface in front of wall for raycast perception
                       {:shape (edge [0 -0.5] [0 5.5])}
                       (for [poi ladder-pois]
                         {:shape (edge poi (v-add poi [-0.5 -0.1]))
                          :friction 1})))
        ;; 1m down, 0.5m up
        boulder-pts [[1.0 -0.4] [0.9 0.3] [0.1 0.5] [-0.6 0.4]
                     [-0.9 -0.1] [-0.7 -0.8] [-0.2 -1.0] [0.6 -1.0]]
        boulder-pois (take-nth 2 (drop 1 boulder-pts))
        boulder-lo (simple-entity
                    boulder-pois
                    (body! world {:position [3 3.5]}
                           {:shape (polygon boulder-pts)
                            :density 5
                            :friction 1})
                    :entity-type :movable)
        boulder-hi (simple-entity
                    boulder-pois
                    (body! world {:position [-1 8]
                                  :angle Math/PI}
                           {:shape (polygon boulder-pts)
                            :density 5
                            :friction 1})
                    :entity-type :movable)
        entities {:ground ground
                  :left-wall left-wall
                  :right-wall right-wall
                  :ceiling ceiling
                  :pedestal pedestal
                  :r-ramp r-ramp
                  :steps steps
                  :l-ramp l-ramp
                  :slope slope
                  :plat plat
                  :ladder ladder
                  :boulder-lo boulder-lo
                  :boulder-hi boulder-hi}
        starting-pts [[-10 4] [9 7] [-3 0] [0 9]]]
    (->
     (games/hunt-game world entities players starting-pts)
     (assoc :game-version [0 0 1]
            :camera {:width (+ (- east-x west-x) 2)
                     :height (+ ceil-y 2)
                     :center [0 (/ ceil-y 2)]}))))

;; =============================================================================
;; ## Vortex Maze
;;
;; Zero gravity. Aim is to push opponent into central vortex. There
;; is a small suction force towards the vortex to avoid getting stuck
;; floating in space. Has enclosing walls around, and four internal
;; barriers.

(defmethod build* :vortex-maze
  [_ players _]
  (let [world (new-world [0 0])
        height 20
        width 24
        half-height (/ height 2)
        half-width (/ width 2)
        v-wall-pts [[0 (- half-height)]
                    [0 half-height]]
        left-wall (simple-entity
                   v-wall-pts
                   (body! world {:type :static
                                 :position [(- half-width) 0]}
                          {:shape (apply edge v-wall-pts)}))
        right-wall (simple-entity
                    v-wall-pts
                    (body! world {:type :static
                                  :position [half-width 0]}
                           {:shape (apply edge v-wall-pts)}))
        h-wall-pts [[(- half-width) 0]
                    [half-width 0]]
        top-wall (simple-entity
                  h-wall-pts
                  (body! world {:type :static
                                :position [0 half-height]}
                         {:shape (apply edge h-wall-pts)}))
        bot-wall (simple-entity
                  h-wall-pts
                  (body! world {:type :static
                                :position [0 (- half-height)]}
                         {:shape (apply edge h-wall-pts)}))
        barrier-fx {:shape (box 3 0.2)
                    :friction 1}
        barrier-pts [[-3 0] [3 0]]
        barriers (for [i (range 4)
                       :let [k (keyword (str "barrier-" i))
                             pos (get [[5 -5]
                                       [-5 -5]
                                       [-5 5]
                                       [5 5]] i)]]
                   [k (simple-entity
                       barrier-pts
                       (body! world {:type :static
                                     :position pos
                                     :angle (* Math/PI (/ i 2))}
                              barrier-fx))])
        vortex (simple-entity
                [[0 0]]
                (body! world {:type :static
                              :position [0 0]
                              :user-data {:org.nfrac.cljbox2d.testbed/rgb
                                          [64 0 64]}}
                       {:shape (circle 1.0)}))
        entities (into {:left-wall left-wall
                        :right-wall right-wall
                        :top-wall top-wall
                        :bottom-wall bot-wall
                        :vortex vortex}
                       barriers)
        vortex-body (ent/entity-body vortex)
        starting-pts [[-8 -1.5] [8 -1.5] [2 5] [-2 -8]]
        starting-vels [[-3 -3] [3 3] [-3 3] [3 -3]]]
    (->
     (games/vortex-game world entities players starting-pts vortex-body)
     (assoc :game-version [0 0 1]
            :camera {:width 32 :height 24 :center [0 0]})
     (games/set-player-vels (keys players) starting-vels))))

