(ns org.nfrac.hatto.core
  (:require [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]))

(defrecord BodyPois [body pois])

(defrecord Entity [entity-type objects joints])

(defrecord PointState [point position velocity angular-velocity])

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

