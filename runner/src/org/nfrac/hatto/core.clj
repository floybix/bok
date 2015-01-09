(ns org.nfrac.hatto.core
  (:require [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [org.nfrac.hatto.creatures :as creatures]
            [org.nfrac.hatto.arena-simple :as arenas]
            [org.nfrac.hatto.data :refer [->PointState]]))

(defn point-state
  [body poi]
  (->PointState poi
                (position body poi)
                (linear-velocity body poi)
                (angular-velocity body)))

(defn observe-entity
  [entity]
  (reduce-kv (fn [m k {:keys [body pois]}]
               (assoc m k (for [poi pois]
                            (point-state body poi))))
             {}
             (:objects entity)))

(defn sense-joints
  [entity inv-dt]
  (reduce-kv (fn [m k jt]
               (assoc m k
                      {:joint-speed (joint-speed jt)
                       :motor-on? (motor-enabled? jt)
                       :motor-speed (motor-speed jt)
                       :motor-torque (motor-torque jt inv-dt)}))
             {}
             (:joints entity)))

(defn perceive
  [state player-key]
  (let [inv-dt (/ 1 (:dt-secs state))
        me (get-in state [:entities player-key])
        my-head (:body (:head (:objects me)))
        eye (position my-head)
        eye-vel (linear-velocity my-head)
        obs (reduce-kv (fn [m k entity]
                         (assoc m k (observe-entity entity)))
                       {}
                       (:entities state))]
    (-> obs
        (assoc :myself {:limbs (obs player-key)
                        :joints (sense-joints me inv-dt)
                        :eye-position eye
                        :eye-velocity eye-vel}
               :time (:time state))
        (dissoc player-key))))

(defn act!
  [entity actions]
  (doseq [[k v] actions]
    (let [jt (get-in entity [:joints k])]
      (enable-motor! jt (boolean v))
      (when v
        (motor-speed! jt v)))))

(defn act-now?
  [{:keys [time dt-act-secs last-act-time]}]
  (>= time (+ dt-act-secs last-act-time)))

(defn take-actions
  [state a-action-fn b-action-fn]
  (if (act-now? state)
    (let [obs-a (perceive state :creature-a)
          obs-b (perceive state :creature-b)
          act-a (a-action-fn obs-a)
          act-b (b-action-fn obs-b)]
      (act! (:creature-a (:entities state)) act-a)
      (act! (:creature-b (:entities state)) act-b)
      (assoc state :last-act-time (:time state)))
    state))

(defn setup-game
  [type-a type-b]
  (let [world (new-world)
        arena (arenas/build! world)
        creature-a (creatures/build type-a world [-10 10] -1)
        creature-b (creatures/build type-b world [10 10] -2)]
    {:world world
     :time 0.0
     :dt-secs (/ 1 30.0)
     :dt-act-secs (/ 1 5.0)
     :last-act-time 0.0
     :camera {:width 40 :height 20 :x-left -20 :y-bottom -5}
     :entities {:arena arena
                :creature-a creature-a
                :creature-b creature-b}}))
