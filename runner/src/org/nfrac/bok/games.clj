(ns org.nfrac.bok.games
  (:require [org.nfrac.bok.entities :as ent :refer [simple-entity]]
            [org.nfrac.bok.creatures :as creatures]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d
             :refer [v-add v-sub polar-xy v-interp v-scale]]))

;; =============================================================================

(defrecord Game
    [game-type
     game-version
     world
     entities
     player-keys
     dead-players
     time
     dt-secs
     ;; method implementations
     perceive
     act
     world-step
     check-end])

;; =============================================================================
;; ## default method implementations

(defn perceive
  [game player-key]
  (let [inv-dt (/ 1 (:dt-secs game))
        me (get-in game [:entities player-key])
        other-players (disj (:player-keys game) player-key)
        obs (->> (:entities game)
                 (map (fn [[k ent]]
                        ;; filter out perception of other players if out of sight
                        (if (and (other-players k)
                                 (not (ent/line-of-sight? (:world game) me
                                                          player-key ent k)))
                          nil
                          ;; otherwise - normal perception
                          [k (ent/perceive-entity ent inv-dt (= k player-key))])))
                 (into {}))]
    {:time (:time game)
     :my-key player-key
     :other-players other-players
     :dead-players (:dead-players game)
     :guns (:player-gun game)
     :energy (:player-energy game)
     :raycast (get-in game [:player-raycast player-key])
     :entities obs}))

(defn act
  [game player-key actions]
  (let [me (get-in game [:entities player-key])]
    (ent/apply-joint-actions! me
                              (:joint-motors actions)
                              (:joint-torques actions))
    (assoc-in game [:player-raycast player-key]
              (ent/raycast-perception (:world game) me player-key
                                      (:raycast actions)))))

(defn world-step
  [game]
  (-> game
      (update-in [:world] step! (:dt-secs game))
      (update-in [:time] + (:dt-secs game))))

(def Inf Double/POSITIVE_INFINITY)

(defn check-dead-or-time-limit
  [game]
  (if (>= (:time game) (:game-over-secs game Inf))
    {:winner nil}
    (let [{:keys [dead-players player-keys]} game]
      (if (>= (count dead-players)
              (dec (count player-keys)))
        {:winner (first (remove dead-players player-keys))}))))

;; =============================================================================

(def empty-game
  (map->Game
   {:entities {}
    :player-keys #{}
    :dead-players #{}
    :time 0.0
    :dt-secs (/ 1 32.0)
    ;; default method implementations
    :perceive perceive
    :act act
    :world-step world-step
    :check-end check-dead-or-time-limit}))

(defn add-players
  "Creates entities for each given player and adds them to the world
   and the game recor. `players` is a map from player keywords to a
   creature type. `starting-pts` is a sequence of [x y] positions."
  [game players starting-pts]
  (let [world (:world game)
        es (map (fn [player-type pt i]
                  (creatures/build player-type world pt (- (inc i))))
                (vals players)
                starting-pts
                (range))]
    (-> game
        (update-in [:entities] into (zipmap (keys players) es))
        (update-in [:player-keys] into (keys players)))))

(defn set-player-vels
  [game player-keys vels]
  (doseq [[k vel] (zipmap player-keys vels)
          body (vals (get-in game [:entities k :components]))]
    (linear-velocity! body vel))
  game)

;; =============================================================================

(defmulti build*
  (fn [type players opts]
    type))

(defn build
  "Returns a Game record where all Body objects have their entity and
   components keys attached in user-data `:org.nfrac.bok/entity`
   and `:org.nfrac.bok/component`."
  [type players opts]
  (let [game (build* type players opts)]
    (doseq [[ent-k ent] (:entities game)
            [cmp-k cmp] (:components ent)]
      (vary-user-data cmp #(assoc % :org.nfrac.bok/entity ent-k
                                  :org.nfrac.bok/component cmp-k)))
    game))

;; =============================================================================
;; ## Sandbox.
;;
;; Just a flat ground and enclosing walls. For testing.

(defmethod build* :sandbox
  [type players _]
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
        starting-pts (map vector [-10 10 0 -5 5] (repeat 10))]
    (-> (assoc empty-game
          :world world
          :game-type type
          :game-version [0 0 1]
          :entities entities
          :camera {:width 40 :height 20 :center [0 5]})
        (add-players players starting-pts))))

;; =============================================================================
;; ## Sumo
;;
;; Just a flat ground/platform, aim is to push opponent off. Times out
;; after 60s.

(defn sumo-step
  [game death-level]
  (let [game (world-step game)
        dead (filter (fn [player-key]
                       (let [player (get-in game [:entities player-key])
                             head (-> player :components :head)
                             [x y] (position head)]
                         ;; head has fallen below death level
                         (< y death-level)))
                     (:player-keys game))]
    (if (seq dead)
      (update-in game [:dead-players] into dead)
      game)))

(defmethod build* :sumo
  [type players _]
  (let [world (new-world)
        ground (simple-entity
                [[-15 0] [15 0]]
                (body! world {:type :static}
                       {:shape (box 15 20 [0 -20])
                        :friction 1}))
        entities {:ground ground}
        starting-pts (map vector [-10 10 0 -5 5] (repeat 10))]
    (->
     (assoc empty-game
       :world world
       :game-type type
       :game-version [0 0 1]
       :entities entities
       :camera {:width 40 :height 20 :center [0 5]}
       :game-over-secs 60.0
       :world-step (fn [game]
                     (sumo-step -2)))
     (add-players players starting-pts))))

;; =============================================================================
;; ## Vortex Maze
;;
;; Zero gravity. Aim is to push opponent into central vortex. There
;; is a small suction force towards the vortex to avoid getting stuck
;; floating in space. Has enclosing walls around, and four internal
;; walls.

(defmethod build* :vortex-maze
  [type players _]
  (let [world (new-world [0 0])
        fence-pois [[-12 -10]
                    [-12 10]
                    [12 10]
                    [12 -10]]
        fence (simple-entity
               fence-pois
               (body! world {:type :static}
                      {:shape (edge-loop fence-pois)
                       :friction 1}))
        wall-fx {:shape (box 3 0.2)
                 :friction 1}
        wall-pois [[-3 0] [3 0]]
        walls (for [i (range 4)
                    :let [k (keyword (str "wall-" i))
                          pos (get [[5 -5]
                                    [-5 -5]
                                    [-5 5]
                                    [5 5]] i)]]
                [k (simple-entity
                    wall-pois
                    (body! world {:type :static
                                  :position pos
                                  :angle (* Math/PI (/ i 2))}
                           wall-fx))])
        vortex (simple-entity
                [[0 0]]
                (body! world {:type :static
                              :position [0 0]
                              :user-data {:org.nfrac.cljbox2d.testbed/rgb
                                          [64 0 64]}}
                       {:shape (circle 1.0)}))
        entities (into {:fence fence
                        :vortex vortex}
                       walls)
        vortex-body (ent/entity-body vortex)
        starting-pts [[-8 0] [8 0]]
        starting-vels [[-3 -3] [3 3]]]
    (->
     (assoc empty-game
       :world world
       :game-type type
       :game-version [0 0 1]
       :entities entities
       :camera {:width 32 :height 24 :center [0 0]}
       :world-step (fn [game]
                     ;; apply suction force towards vortex
                     (let [vort-pos (position vortex-body)]
                       (doseq [k (:player-keys game)
                               body (vals (get-in game [:entities k :components]))
                               :let [pos (center body)
                                     force (-> (v-sub vort-pos pos)
                                               (v-scale)
                                               (v-scale 0.5))]]
                         (apply-force! body force (loc-center body))))
                     ;; do standard step
                     (let [game (world-step game)
                           ;; kill players touching vortex
                           player-keys (:player-keys game)
                           dead (distinct
                                 (map (comp player-keys ::entity user-data)
                                      (contacting vortex-body)))]
                       (if (seq dead)
                         (update-in game [:dead-players] into dead)
                         game))))
     (add-players players starting-pts)
     (set-player-vels (keys players) starting-vels))))

;; =============================================================================
;; ## Energy Race
;;
;; Aim is to have more energy than opponent when the time limit is
;; reached. Touch the scattered food particles with head to eat them
;; and gain energy. Moving joints uses energy. Arena is a x^4 curve
;; with a sine wave mixed in for foot-holds. Also two high platforms.

(defn check-max-energy
  [game]
  (when (>= (:time game) (:game-over-secs game Inf))
    (let [[winner energy] (apply max-key val (:player-energy game))]
      {:winner winner
       :energy (:player-energy game)})))

(defn energy-race-step
  [game]
  (let [world (:world game)]
    ;; if any food is touched, dislodge it - switch on its gravity
   (doseq [cd (all-current-contacts world)
           fixt [(:fixture-a cd) (:fixture-b cd)]
           :let [body (body-of fixt)]]
     (when (:food-joules (user-data body))
       (gravity-scale! body 1.0)))
   ;; do energy accounting
   (-> (reduce
        (fn [game player-key]
          (let [me (get-in game [:entities player-key])
                e-loss (ent/entity-work-joules me (:dt-secs game))
                head (get-in me [:components :head])
                snack (first (filter (comp :food-joules user-data)
                                     (contacting head)))
                snack-k (when snack (:org.nfrac.bok/entity (user-data snack)))
                e-gain (if snack (:food-joules (user-data snack)) 0)
                energy (+ (get-in game [:player-energy player-key])
                          (- e-gain e-loss))]
            (when snack
              (destroy! snack))
            (-> (if snack
                  (update-in game [:entities] dissoc snack-k)
                  game)
                (update-in [:player-energy] assoc player-key energy))))
        game
        (:player-keys game))
       (world-step))))

(defmethod build* :energy-race
  [type players _]
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
        starting-pts (map vector [-10 10 0 -5 5] (repeat 10))]
    (->
     (assoc empty-game
       :world world
       :game-type type
       :game-version [0 0 1]
       :entities entities
       :camera {:width 40 :height 22 :center [0 10]}
       :player-energy (zipmap (keys players) (repeat 300.0))
       :game-over-secs 60.0
       :world-step energy-race-step
       :check-end check-max-energy)
     (add-players players starting-pts))))

;; =============================================================================
;; ## Altitude
;;
;; Aim is to be at a higher (head) position than opponent when the
;; time limit is reached. Arena has large and small blocks, two
;; platforms, a central swing (on rope joints) and a stalactite with
;; hand-holds.

(defn check-highest
  [game]
  (when (>= (:time game) (:game-over-secs game Inf))
    (let [heights (->>
                   (map (fn [player-key]
                          (let [player (get-in game [:entities player-key])
                                head (-> player :components :head)
                                [x y] (position head)]
                            [player-key y]))
                        (:player-keys game))
                   (into {}))
          highest (apply max-key heights (:player-keys game))]
      {:winner highest
       :heights heights})))

(defmethod build* :altitude
  [type players _]
  (let [world (new-world)
        ground-pts [[-16 0] [-4 0] [0 -2] [4 0] [16 0]]
        ground (simple-entity
                ground-pts
                (body! world {:type :static}
                       {:shape (edge-chain ground-pts)
                        :friction 1}))
        ceil-y 16
        ceiling (simple-entity
                 [[-16 0] [16 0]]
                 (body! world {:type :static
                               :position [0 ceil-y]}
                        {:shape (edge [-16 0] [-3 0])}
                        {:shape (edge [3 0] [16 0])}))
        stal-depth 6
        stal-yend 10
        stal-xoff 1
        stal-pois (for [sign [-1 1]
                        frac (range 0.1 0.9 0.2)]
                    (v-interp [0 (- stal-depth)]
                              [(* sign stal-xoff) 0]
                              frac))
        stalactite (simple-entity
                    stal-pois
                    (apply body! world {:type :static
                                        :position [0 ceil-y]}
                           {:shape (polygon [[stal-xoff 0]
                                             [(- stal-xoff) 0]
                                             [0 (- stal-depth)]])
                            :friction 1}
                           (for [pos stal-pois
                                 :let [sign (if (neg? (first pos)) -1 1)]]
                             {:shape (edge pos (v-add pos [(* sign 0.5) 0.1]))
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
        starting-pts (map vector [-10 10] (repeat 2.5))]
    (->
     (assoc empty-game
       :world world
       :game-type type
       :game-version [0 0 1]
       :entities entities
       :camera {:width 40 :height 20 :center [0 7]}
       :game-over-secs 60.0
       :check-end check-highest)
     (add-players players starting-pts))))

;; =============================================================================
;; ## Hunt
;;
;; Aim is to fire a bullet hitting your opponent. Each player has a
;; gun attached to their head. It does not collide with things, but it
;; has some angle at which it is pointed. There are special actions to
;; rotate the gun and to fire the gun. There is a maximum speed at
;; which the gun can be rotated, so it can take several time steps to
;; aim it.

(def GUN_SPEED 1.0)
(def GUN_AMMO 50)
(def GUN_RELOAD 2) ;; seconds

(defn hunt-act
  [game player-key actions]
  ;; process usual actions first
  (let [game (act game player-key actions)
        world (:world game)
        gun-info (get-in game [:player-gun player-key])
        ang (:angle gun-info)]
    (if (and (:fire (:gun actions))
             (zero? (:reload-countdown gun-info))
             (pos? (:ammo gun-info)))
      ;; fire gun
      (let [head (get-in game [:entities player-key :components :head])
            group-index (get-in game [:entities player-key :group-index])
            bullet (body! world {:position (v-add (position head)
                                                  (polar-xy 0.5 ang))
                                 :angle ang
                                 :bullet true}
                          {:shape (box 0.2 0.05)
                           :density 20
                           :group-index group-index
                           :user-data {::bullet-of player-key}})
            impulse (polar-xy 25 ang)]
        (apply-impulse! bullet impulse (center bullet))
        (apply-impulse! head (v-scale impulse -1) (center head))
        (-> game
            (update-in [:player-gun player-key :ammo] dec)
            (assoc-in [:player-gun player-key :reload-countdown]
                      GUN_RELOAD)))
      ;; otherwise, not firing gun
      (let [dt (:dt-secs game)
            da (-> (:speed (:gun actions) 0)
                   (min GUN_SPEED)
                   (max (- GUN_SPEED))
                   (* dt))]
        (-> game
            (update-in [:player-gun player-key :angle] + da)
            (update-in [:player-gun player-key :reload-countdown]
                       #(max 0 (- % dt))))))))

(defn hunt-step
  [game]
  ;; do standard step first:
  (let [game (world-step game)
        contacts (:buffered-contacts game)
        hits (keep (fn [{:keys [fixture-a fixture-b]}]
                     (cond
                      (::bullet-of (user-data fixture-a))
                      [fixture-a fixture-b]
                      (::bullet-of (user-data fixture-b))
                      [fixture-b fixture-a]))
                   @contacts)
        dead (distinct
              (keep (fn [[_ target-fixt]]
                      (->> (body-of target-fixt)
                           (user-data)
                           :org.nfrac.bok/entity
                           (get (:player-keys game))))
                    hits))]
    (doseq [[bullet-fixt _] hits]
      (destroy! (body-of bullet-fixt)))
    (swap! contacts empty)
    (if (seq dead)
      (update-in game [:dead-players] into dead)
      game)))

(defmethod build* :hunt
  [type players _]
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
        starting-pts [[-10 8] [10 8]]]
    (->
     (assoc empty-game
       :world world
       :game-type type
       :game-version [0 0 1]
       :entities entities
       :camera {:width (+ (- east-x west-x) 2)
                :height (+ ceil-y 2)
                :center [0 (/ ceil-y 2)]}
       :player-gun (zipmap (keys players)
                           (repeat {:angle 0.0
                                    :ammo GUN_AMMO
                                    :reload-countdown GUN_RELOAD}))
       :buffered-contacts (set-buffering-contact-listener! world)
       :act hunt-act
       :world-step hunt-step)
     (add-players players starting-pts))))
