(ns org.nfrac.hatto.games
  (:require [org.nfrac.hatto.entities :as ent :refer [with-pois map->Entity]]
            [org.nfrac.hatto.creatures :as creatures]
            [cljbox2d.core :refer :all]))

;; =============================================================================

(defrecord Game
    [game-type
     game-attributes
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
