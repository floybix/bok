(ns org.nfrac.hatto.core
  (:require [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [v-dist v-sub v-scale v-angle v-dist]]))

(defrecord BodyPois [body pois])

(defrecord PoiState
    [poi
     position
     velocity
     angular-velocity])

(defn poi-state
  [body poi]
  (->PoiState poi
              (position body poi)
              (linear-velocity body poi)
              (angular-velocity body)))

(defn observe-entity
  [entity]
  (reduce-kv (fn [m k {:keys [body pois]}]
               (assoc m k (for [poi pois]
                            (poi-state body poi))))
             {}
             (:limbs entity)))

(defn infer-derived-measures
  [^PoiState info eye eye-vel]
  (let [pos (:position info)
        vel (:velocity info)
        rel-pos (v-sub pos eye)
        rel-vel (v-sub vel eye-vel)]
    (assoc info
      :relative-position rel-pos
      :angle-from-me (v-angle rel-pos)
      :distance (v-dist pos eye)
      :relative-velocity rel-vel
      :velocity-angle-to-me (v-angle (v-scale (v-sub rel-vel rel-pos) -1)))))

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

(defn act!
  [entity actions]
  (doseq [[k v] actions]
    (let [jt (get-in entity [:joints k])]
      (enable-motor! jt (boolean v))
      (when v
        (motor-speed! jt v)))))

(defn post-step
  [state]
  (let [{:keys [player-a player-b]} (:game state)
        inv-dt (/ 1 (:dt-secs state))]
    (doseq [[player opponent] [[player-a player-b]
                               [player-b player-a]]]
      (let [my-head (:body (:head (:limbs player)))
            eye (position my-head)
            eye-vel (linear-velocity my-head)
            action-fn (:action-fn player)
            my-info {:opponent (observe-entity opponent)
                     :myself {:limbs (observe-entity player)
                              :joints (sense-joints player inv-dt)
                              :eye-position eye
                              :eye-velocity eye-vel}}
            my-actions (action-fn my-info)]
        (act! player my-actions)))
    state))
