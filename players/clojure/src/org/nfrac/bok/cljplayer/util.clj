(ns org.nfrac.bok.cljplayer.util)

(def PI Math/PI)
(def HALF_PI (/ PI 2))

(defn abs [x] (if (neg? x) (- x) x))
(defn sign [x] (if (neg? x) -1 1))

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

(def angle-up? pos?)

(defn angle-horiz?
  [a epsilon]
  (or (< (abs a) epsilon)
      (> (abs a) (- Math/PI epsilon))))

(defn turn-towards
  "Returns a motor speed in radians per second (positive being
   counter-clockwise, negative clockwise) to turn to `target-angle`
   from the current angle `ang`. Speed is linear from a maximum of
   `s-max` down to zero.

   This assumes that the joints have been constructed so that the
   joint angle is aligned with the given angle values (presumably
   world angles)."
  [target-angle ang s-max]
  (let [ang-diff (loop [d (- target-angle ang)]
                   (cond
                    (> d PI) (recur (- d (* 2 PI)))
                    (< d (- PI)) (recur (+ d (* 2 PI)))
                    :else d))]
    (* ang-diff s-max (/ PI))))

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
