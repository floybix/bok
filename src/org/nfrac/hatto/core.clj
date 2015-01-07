(ns org.nfrac.hatto.core
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [v-dist v-sub v-scale v-angle v-dist]]
            [quil.core :as quil]
            [quil.middleware]
            [clojure.pprint]))

(defn step-a
  [info]
  ;(clojure.pprint/pprint info)
  {:limb-a-rj -5
   :limb-b-rj 4})

(defn act!
  [entity actions]
  (doseq [[k v] actions]
    (let [jt (get-in entity [:joints k])]
      (enable-motor! jt (boolean v))
      (when v
        (motor-speed! jt v)))))

(defrecord BodyPois [body pois])

(defrecord PoiState
    [poi
     position
     velocity
     angular-velocity])

(defn poi-state
  [body poi]
  (PoiState. poi
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

(defn player-nin
  [world position group-index]
  (let [head (body! world {:position position}
                    {:shape (circle 0.5)
                     :density 10
                     :group-index group-index})
        tri-fx {:shape (polygon [[0.0 0.3]
                                 [0.9 -0.4]
                                 [-0.9 -0.4]])
                :density 10
                :group-index group-index}
        tri-pois [[0.9 -0.4]
                  [-0.9 -0.4]]
        limb-a (body! world {:position position}
                      tri-fx)
        limb-b (body! world {:position position}
                      tri-fx)
        rj-a (joint! {:type :revolute
                      :body-a limb-a
                      :body-b head
                      :world-anchor position
                      :max-motor-torque 1000})
        rj-b (joint! {:type :revolute
                      :body-a limb-b
                      :body-b head
                      :world-anchor position
                      :max-motor-torque 1000})]
    {:entity-type :nin
     :limbs {:head (BodyPois. head [[0 0]])
             :limb-a (BodyPois. limb-a tri-pois)
             :limb-b (BodyPois. limb-b tri-pois)}
     :joints {:limb-a-rj rj-a
              :limb-b-rj rj-b}}))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (box 15 20 [0 -20])})
        player-a (player-nin world [-10 10] -1)
        player-b (player-nin world [10 10] -2)]
    (assoc bed/initial-state
      :world world
      :camera {:width 40 :height 20 :x-left -20 :y-bottom -5}
      ::my {:player-a player-a
            :player-b player-b})))

(defn post-step
  [state]
  (let [{:keys [player-a player-b]} (::my state)
        obs-a (observe-entity player-a)
        obs-b (observe-entity player-b)
        inv-dt (/ 1 (:dt-secs state))
        j-info-a (sense-joints player-a inv-dt)]
    (let [my-head (:body (:head (:limbs player-a)))
          eye (position my-head)
          eye-vel (linear-velocity my-head)
          act-a (step-a {:opponent obs-b
                         :myself {:eye-position eye
                                  :eye-velocity eye-vel
                                  :limbs obs-a
                                  :joints j-info-a}})]
      (act! player-a act-a))
    ;; TODO act-b
    state))

(defn step
  [state]
  (if (:paused? state)
    state
    (-> (update-in state [:world] step! (:dt-secs state))
        (post-step))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Hatto"
    :setup setup
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [1200 600]
    :middleware [quil.middleware/fun-mode]))

