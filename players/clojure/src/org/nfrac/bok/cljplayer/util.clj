(ns org.nfrac.bok.cljplayer.util)

(def PI Math/PI)
(def HALF_PI (/ PI 2))

(defn abs [x] (if (neg? x) (- x) x))
(defn sign [x] (if (neg? x) -1 1))

(defn x-val [[x y]] x)
(defn y-val [[x y]] y)

(defn v-angle
  "Angle of a 2d geometric vector in radians in range -pi to pi."
  [[x y]]
  (Math/atan2 y x))

(defn v-mag
  "Magnitude of a 2d geometric vector."
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn v-sub
  "Subtract a 2d geometric vector from another (v1 - v2)."
  [v1 v2]
  (mapv - v1 v2))

(defn v-scale
  "Multiply elements of a 2d vector by a scalar."
  ([[x y] s]
     [(* x s) (* y s)]))

(defn v-dist
  "Distance from one 2d point to another."
  [v1 v2]
  (v-mag (v-sub v1 v2)))

(defn angle-left? [a] (> (abs a) HALF_PI))

(defn angle-right? [a] (not (angle-left? a)))

(def angle-up? pos?)

(defn angle-horiz?
  [a epsilon]
  (or (< (abs a) epsilon)
      (> (abs a) (- Math/PI epsilon))))

(defn turn-towards
  "Returns a joint control value (`[motor-speed max-torque]`) for
   turning towards a target angle, given the current angle and angular
   velocity. The motion is to be taken at the given `speed` but slows
   within pi/8 of the target angle. The torque (limit) is calculated
   as a Proportional Derivative controller: (`k_p` X `ang-diff`)
   - (`k_d` X `ang-vel`)."
  ([target-ang curr-ang ang-vel speed]
     (turn-towards target-ang curr-ang ang-vel speed 100 10))
  ([target-ang curr-ang ang-vel speed k_p k_d]
     (let [d (- target-ang curr-ang)
           ang-diff (cond
                     (> d PI) (- d (* 2 PI))
                     (< d (- PI)) (+ d (* 2 PI))
                     :else d)
           ;; reduce speed to zero near target angle
           eps (/ PI 8)
           sp (* speed (-> (/ ang-diff eps)
                           (min 1.0)
                           (max -1.0)))
           tq (- (* k_p ang-diff) (* k_d ang-vel))]
       [sp (abs tq)])))

(defn point-features
  [point eye]
  (let [{pos :position, vel :velocity} point
        {eye-pos :position, eye-vel :velocity} eye
        rel-pos (v-sub pos eye-pos)
        rel-vel (v-sub vel eye-vel)]
    (assoc point
      :relative-position rel-pos
      :angle-from-me (v-angle rel-pos)
      :distance (v-dist pos eye-pos)
      :relative-velocity rel-vel
      :velocity-angle-to-me (v-angle (v-scale (v-sub rel-vel rel-pos) -1)))))
