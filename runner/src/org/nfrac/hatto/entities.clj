(ns org.nfrac.hatto.entities
  (:require [org.nfrac.cljbox2d.core :refer :all]))

(defrecord Entity [entity-type components joints])

(defrecord PointState [point position velocity])

(defn with-pois
  [body pois]
  (vary-user-data body #(assoc % :points-of-interest pois)))

(defn entity-mass
  [entity]
  (->> entity
       :components
       vals
       (map mass)
       (reduce +)))

(defn entity-angular-velocity
  [entity]
  (let [total-mass (entity-mass entity)]
    (if (zero? total-mass)
      0.0
      (->> entity
           :components
           vals
           (map (fn [body]
                  (* (angular-velocity body)
                     (/ (mass body) total-mass))))
           (reduce +)))))

(defn entity-work-joules
  [entity dt]
  (->> entity
       :joints
       vals
       (map power-watts)
       (reduce +)
       (* dt)))

(defn point-state
  [body poi]
  (->PointState poi
                (position body poi)
                (linear-velocity body poi)))

(defn observe-components
  [entity]
  (reduce-kv (fn [m k body]
               (let [pois (:points-of-interest (user-data body))]
                 (assoc m k
                        {:angle (angle body)
                         :points (for [poi pois]
                                   (point-state body poi))})))
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

(defn set-joint-motors!
  [entity joint-actions]
  (doseq [[k jt] (:joints entity)]
    (let [v (get joint-actions k)]
      (do
        (enable-motor! jt (boolean v))
        (when v
          (motor-speed! jt v))))))
