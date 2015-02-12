(ns org.nfrac.bok.entities
  (:require [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [v-add v-scale polar-xy]]))

(def ENTITY_TYPES #{:fixed :movable :food :creature})

(defrecord Entity [entity-type components joints])

(defrecord PointState [point position velocity])

(defn set-pois
  [body pois]
  (vary-user-data body #(assoc % :points-of-interest pois)))

(defn entity
  [components & {:keys [entity-type]
                 :or {entity-type :fixed}
                 :as attrs}]
  {:pre (ENTITY_TYPES entity-type)}
  (map->Entity (assoc attrs
                 :components components
                 :entity-type entity-type)))

(defn simple-entity
  [pois body & more-args]
  (let [components {:it (set-pois body pois)}]
    (apply entity components more-args)))

(defn entity-body
  "For simple entities, returns the single component Body."
  [ent]
  (first (vals (:components ent))))

(defn entity-mass
  [entity]
  (->> entity
       :components
       vals
       (map mass)
       (reduce +)))

(defn entity-center-of-mass
  [entity]
  (let [total-mass (entity-mass entity)]
    (if (zero? total-mass)
      (center (entity-body entity))
      (->> entity
           :components
           vals
           (map (fn [body]
                  (v-scale (center body)
                           (/ (mass body) total-mass))))
           (reduce v-add)
           (mapv float)))))

(defn entity-velocity
  [entity]
  (let [total-mass (entity-mass entity)]
    (if (zero? total-mass)
      (linear-velocity (entity-body entity))
      (->> entity
           :components
           vals
           (map (fn [body]
                  (v-scale (linear-velocity body)
                           (/ (mass body) total-mass))))
           (reduce v-add)
           (mapv float)))))

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
           (reduce +)
           (float)))))

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

(defn sense-contacts
  [body]
  (map (fn [cd]
         (let [other (if (= body (body-of (:fixture-a cd)))
                       (body-of (:fixture-b cd))
                       (body-of (:fixture-a cd)))]
           {:entity (:org.nfrac.bok/entity (user-data other))
            :points (:points cd)
            :normal (:normal cd)}))
       (current-contacts body)))

(defn observe-components
  [entity self?]
  (reduce-kv (fn [m k body]
               (let [pois (:points-of-interest (user-data body))
                     info {:angle (angle body)
                           :ang-vel (angular-velocity body)
                           :points (for [poi pois]
                                     (point-state body poi))}]
                 (assoc m k
                        (if-let [cs (and self? (seq (sense-contacts body)))]
                          (assoc info :contacts cs)
                          info))))
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

(defn line-of-sight?
  [world me my-id other other-id]
  (let [eye (-> me :components :head position)
        oth-eye (-> other :components :head position)
        rc (first
            (raycast world eye oth-eye :closest
                     :ignore (fn [fixt]
                               (let [id (-> fixt body-of user-data
                                            :org.nfrac.bok/entity)]
                                 ;; might be nil for extras, like bullets
                                 (or (nil? id) (= id my-id))))))
        hit-ent-id (-> rc :fixture body-of user-data :org.nfrac.bok/entity)]
    (when-not hit-ent-id
      (println "line-of-sight? raycast returned nothing:" rc))
    (= other-id hit-ent-id)))

(defn perceive-entity
  [entity inv-dt self?]
  (cond->
   {:type (:entity-type entity)
    :components (observe-components entity self?)
    :center-of-mass (entity-center-of-mass entity)
    :velocity (entity-velocity entity)
    :ang-vel (entity-angular-velocity entity)}
   self?
   (assoc :joints (sense-joints entity inv-dt))))

(defn raycast-perception
  [world me my-id rc-action]
  (when-let [ang rc-action]
    (let [eye (-> me :components :head position)
          extent 100.0
          to (v-add eye (polar-xy extent ang))
          rc (first
              (raycast world eye to :closest
                       :ignore (fn [fixt]
                                 (let [id (-> fixt body-of user-data
                                              :org.nfrac.bok/entity)]
                                   ;; might be nil for extras, like bullets
                                   (or (nil? id) (= id my-id))))))
          hit-ent-id (when rc
                       (-> rc :fixture body-of user-data :org.nfrac.bok/entity))]
      {:angle ang
       :entity hit-ent-id
       :distance (when rc (float (* (:fraction rc) extent)))})))

(def MAX_TORQUE 200.0)

(defn apply-joint-actions!
  [entity motor-actions torque-actions]
  (doseq [[k jt] (:joints entity)]
    (let [[sp mmt] (get motor-actions k)]
      (enable-motor! jt (boolean sp))
      (when sp
        (motor-speed! jt sp)
        (max-motor-torque! jt (-> mmt (min MAX_TORQUE)))))
    (when-let [tq* (get torque-actions k)]
      (let [tq (-> tq* (min MAX_TORQUE)
                   (max (- MAX_TORQUE)))]
        (apply-torque! (body-b jt) tq)
        (apply-torque! (body-a jt) (- tq))))))
