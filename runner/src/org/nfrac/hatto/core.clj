(ns org.nfrac.hatto.core
  (:require [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [org.nfrac.hatto.creatures :as creatures]
            [org.nfrac.hatto.arenas :as arenas]
            [org.nfrac.hatto.data :refer [->PointState]]))

(defn entity-mass
  [entity]
  (->> entity
       :components
       vals
       (map (comp mass :body))
       (reduce +)))

(defn entity-angular-velocity
  [entity]
  (let [total-mass (entity-mass entity)]
    (if (zero? total-mass)
      0.0
      (->> entity
           :components
           vals
           (map :body)
           (reduce (fn [av body]
                     (+ av (* (angular-velocity body)
                              (/ (mass body) total-mass))))
                   0)))))

(defn point-state
  [body poi]
  (->PointState poi
                (position body poi)
                (linear-velocity body poi)))

(defn observe-components
  [entity]
  (reduce-kv (fn [m k {:keys [body pois]}]
               (assoc m k
                      {:angle (angle body)
                       :points (for [poi pois]
                                 (point-state body poi))}))
             {}
             (:components entity)))

(defn sense-joints
  [entity inv-dt]
  (reduce-kv (fn [m k jt]
               (assoc m k
                      {:joint-angle (joint-angle jt)
                       :joint-speed (joint-speed jt)
                       :motor-speed (when (motor-enabled? jt) (motor-speed jt))
                       :motor-torque (motor-torque jt inv-dt)}))
             {}
             (:joints entity)))

(defn perceive-entity
  [entity inv-dt]
  {:components (observe-components entity)
   :joints (sense-joints entity inv-dt)
   :angular-velocity (entity-angular-velocity entity)})

(defn perceive
  [game player-key]
  (let [inv-dt (/ 1 (:dt-secs game))
        me (get-in game [:entities player-key])
        obs (reduce-kv (fn [m k entity]
                         (assoc m k (perceive-entity entity inv-dt)))
                       {}
                       (:entities game))]
    {:time (:time game)
     :my-key player-key
     :entities obs}))

(defn act!
  [entity actions]
  (doseq [[k v] actions]
    (if-let [jt (get-in entity [:joints k])]
      (do
        (enable-motor! jt (boolean v))
        (when v
          (motor-speed! jt v)))
      (println "Joint" k "does not exist."))))

(defn act-now?
  [{:keys [time dt-act-secs last-act-time]}]
  (>= time (+ dt-act-secs last-act-time)))

(defn take-actions
  [game a-action-fn b-action-fn]
  (if (act-now? game)
    (let [obs-a (perceive game :creature-a)
          obs-b (perceive game :creature-b)
          act-a (a-action-fn obs-a)
          act-b (b-action-fn obs-b)]
      (act! (:creature-a (:entities game)) act-a)
      (act! (:creature-b (:entities game)) act-b)
      (assoc game :last-act-time (:time game)))
    game))

(defn final-result
  [game]
  (if (> (:time game)
         (:timeout-secs game))
    {:winner nil}
    (let [dead (some (fn [player-key]
                       (let [player (get-in game [:entities player-key])
                             [x y] (-> player :components :head :body position)]
                         (when (< y -2.0)
                           ;; head has fallen 2m below ground level
                           player-key)))
                     (:player-keys game))]
      (when dead
        {:winner (first (disj (:player-keys game) dead))}))))

(defn setup-game
  [arena-type type-a type-b]
  (let [world (new-world)
        arena (arenas/build arena-type world)
        creature-a (creatures/build type-a world [-10 10] -1)
        creature-b (creatures/build type-b world [10 10] -2)]
    {:world world
     :time 0.0
     :timeout-secs Double/POSITIVE_INFINITY
     :dt-secs (/ 1 30.0)
     :dt-act-secs (/ 1 15.0)
     :last-act-time 0.0
     :camera {:width 40 :height 20 :x-left -20 :y-bottom -5}
     :player-keys #{:creature-a :creature-b}
     :entities {:arena arena
                :creature-a creature-a
                :creature-b creature-b}}))
