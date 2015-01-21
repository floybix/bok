(ns org.nfrac.hatto.games
  (:require [org.nfrac.hatto.entities :as ent :refer [with-pois map->Entity]]
            [org.nfrac.hatto.creatures :as creatures]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [v-add polar-xy]]))

;; =============================================================================

(defrecord Game
    [game-type
     game-attributes
     game-version
     world
     entities
     player-keys
     time
     dt-secs
     ;; method implementations
     perceive
     act
     world-step
     check-end])

;; =============================================================================

(defn perceive
  [game player-key]
  (let [inv-dt (/ 1 (:dt-secs game))
        me (get-in game [:entities player-key])
        obs (reduce-kv (fn [m k entity]
                         (assoc m k (ent/perceive-entity entity inv-dt)))
                       {}
                       (:entities game))]
    {:time (:time game)
     :my-key player-key
     :entities obs}))

(defn act-now?
  [{:keys [time dt-act-secs last-act-time]}]
  (>= time (+ dt-act-secs last-act-time)))

(defn act-on-joints
  [game player-key actions]
  (ent/set-joint-motors! (get-in game [:entities player-key])
                         (:joints actions))
  game)

(defn world-step
  [game]
  (-> game
      (update-in [:world] step! (:dt-secs game))
      (update-in [:time] + (:dt-secs game))))

(defn check-time-limit
  [game limit-secs]
  (when (> (:time game) limit-secs)
    {:winner nil}))

(defn check-fallen-down
  [game y-val]
  (let [dead (filter (fn [player-key]
                       (let [player (get-in game [:entities player-key])
                             head (-> player :components :head)
                             [x y] (position head)]
                         ;; head has fallen below death level
                         (< y y-val)))
                     (:player-keys game))]
    (when (seq dead)
      {:winner (first (apply disj (:player-keys game) dead))})))

(defn check-highest
  [game limit-secs]
  (when (> (:time game) limit-secs)
    (let [heights (into {}
                        (map (fn [player-key]
                               (let [player (get-in game [:entities player-key])
                                     head (-> player :components :head)
                                     [x y] (position head)]
                                 [player-key y]))
                             (:player-keys game)))
          highest (apply max-key heights (:player-keys game))]
      {:winner highest
       :heights heights})))

;; =============================================================================

(def empty-game
  (map->Game
   {:entities {}
    :player-keys #{}
    :time 0.0
    :dt-secs (/ 1 30.0)
    :dt-act-secs (/ 1 15.0)
    :last-act-time 0.0
    :perceive perceive
    :act act-on-joints
    :world-step world-step
    :check-end (fn [game] nil)}))

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

;; =============================================================================

(defmulti build
  "Returns a Game record."
  (fn [type players opts]
    type))

(defmethod build :sandbox
  [type players _]
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (box 15 20 [0 -20])
                       :friction 1.0}
                      {:shape (edge [-15 0] [-15 15])}
                      {:shape (edge [15 0] [15 15])})
        pois [[-15 0] [15 0]]
        arena (map->Entity
               {:entity-type :arena
                :arena-type type
                :components {:ground (with-pois ground pois)}})
        starting-pts (map vector [-10 10 0 -5 5] (repeat 10))]
    (-> (assoc empty-game
          :world world
          :entities {:arena arena}
          :camera {:width 40 :height 20 :x-left -20 :y-bottom -5})
        (add-players players starting-pts))))

(defmethod build :sumo
  [type players _]
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (box 15 20 [0 -20])
                       :friction 1.0})
        pois [[-15 0] [15 0]]
        arena (map->Entity
               {:entity-type :arena
                :arena-type type
                :components {:ground (with-pois ground pois)}})
        starting-pts (map vector [-10 10 0 -5 5] (repeat 10))]
    (->
     (assoc empty-game
       :world world
       :entities {:arena arena}
       :camera {:width 40 :height 20 :x-left -20 :y-bottom -5}
       :check-end (fn [game]
                    (or (check-time-limit game 60)
                        (check-fallen-down game -2))))
     (add-players players starting-pts))))

(defn edge-chain
  [vertices attrs]
  (for [[v0 v1] (partition 2 1 vertices)]
    (merge {:shape (edge v0 v1)} attrs)))

(defmethod build :energy-race
  [type players _]
  (let [world (new-world)
        surface (for [x (range -20 20.01 0.2)]
                  [x (+ (/ (Math/pow x 4)
                           (Math/pow 20 3))
                        (* (Math/cos (* x 4))
                           (/ (* x x) (* 20 20))
                           1.5))])
        pois (for [z [0 1/3 2/3 1]]
               (nth surface (* z (dec (count surface)))))
        ground (apply body! world {:type :static}
                      (edge-chain surface
                                  {:friction 1}))
        plat-fx {:shape (edge [-5 0] [5 0])
                 :friction 1}
        plat-pois [[-5 0] [5 0]]
        plat-right (body! world {:type :static
                                 :position [8 15]}
                          plat-fx)
        plat-left (body! world {:type :static
                                :position [-8 15]}
                         plat-fx)
        arena (map->Entity
               {:entity-type :arena
                :arena-type type
                :components {:ground (with-pois ground pois)
                             :plat-left (with-pois plat-left plat-pois)
                             :plat-right (with-pois plat-right plat-pois)}})
        ;; place food by scanning down from above at regular intervals
        food-pos (for [sign [-1 1]
                       xa (concat [2 4 6]
                                  (range 10.5 20))
                       :let [x (* sign xa)]]
                   (let [[rc] (raycast world [x 20] [x -10] :closest)]
                     (if rc
                       (v-add (:point rc) [0 0.2])
                       (do (println (str "raycast missed: " x))
                           [0 0]))))
        food-fx {:shape (polygon [[0 0.5] [-0.25 0] [0.25 0]])
                 :density 5 :restitution 0 :friction 1}
        food (map->Entity
              {:entity-type :food
               :components (into {}
                                 (map-indexed (fn [i pos]
                                                (let [k (keyword (str "food-" i))]
                                                  [k(with-pois
                                                      (body! world {:position pos}
                                                             food-fx)
                                                      [[0 0]])]))
                                              food-pos))})
        starting-pts (map vector [-10 10 0 -5 5] (repeat 10))]
    (->
     (assoc empty-game
       :world world
       :entities {:arena arena
                  :food food}
       :camera {:width 40 :height 20 :x-left -20 :y-bottom -1}
       :check-end (fn [game]
                    (check-highest game 60)))
     (add-players players starting-pts))))
