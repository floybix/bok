(ns org.nfrac.hatto.cljplayer.util)

(def HALF_PI (/ Math/PI 2))

(defn abs [x] (if (neg? x) (- x) x))

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
