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
   as a Proportional Derivative controller: (`k-p` X `ang-diff`)
   - (`k-d` X `ang-vel`)."
  ([target-ang curr-ang ang-vel speed]
     (turn-towards target-ang curr-ang ang-vel speed 100 10))
  ([target-ang curr-ang ang-vel speed k-p k-d]
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
           tq (- (* k-p ang-diff) (* k-d ang-vel))]
       [sp (abs tq)])))

(defn reverse-turn-towards
  "Same as `turn-towards` but for controlling a joint with respect to
   its parent limb. E.g. controlling a hip joint to target a stable
   upright torso. In this case the direction of the joint motor should
   be reversed."
  ([target-ang curr-ang ang-vel speed]
     (reverse-turn-towards target-ang curr-ang ang-vel speed 100 10))
  ([target-ang curr-ang ang-vel speed k-p k-d]
     (turn-towards 0 (- target-ang curr-ang) ang-vel speed k-p k-d)))

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

(defn median
  [xs]
  (nth (sort xs) (quot (count xs) 2)))

(defn slope-check
  "Helps to work out the slope of the ground. `cmps` is the perception
   data for the player's own body components. `prev-state` is this
   function's result from the previous time step. Returns keys

  * `:tangent` - the angle of the ground, derived from (the median of)
   any contact points, where 0 is horizontal, positive slopes up to
   the right and negative slopes down to the right.

  * `:normal` - the angle perpendicular to the tangent."
  [cmps prev-state]
  (let [normals (mapcat (fn [cmp]
                          (when-let [cs (:contacts cmp)]
                            (map :normal cs)))
                        (vals cmps))
        normal (if (seq normals)
                 (->> normals
                      (map v-angle)
                      (median))
                 ;; no contacts
                 (:normal prev-state HALF_PI))]
    ;; (assuming static body, normal is perpendicular to it)
    ;; convert normal to tangent deviation from horizontal:
    {:normal normal
     :tangent (- normal HALF_PI)}))

(def down-left (* PI -0.8))
(def down-right (* PI -0.2))

(defn upward-check
  "Helps to work out which direction to go in to get higher up. `rcs`
   is the raycast perception data, assumed to include angles down-left
   and down-right. Returns keys

   * `:dir` - which direction seems to be upwards: -1 left, 1 right, 0
     not yet determined.

   * `:next-angles` - the angles for the next raycast."
  [rcs]
  (let [left-rc (first (filter #(< (abs (- down-left (:angle %))) 0.5) rcs))
        right-rc (first (filter #(< (abs (- down-right (:angle %))) 0.5) rcs))
        left (when left-rc (or (:distance left-rc) 100))
        right (when right-rc (or (:distance right-rc) 100))]
    {:left left
     :right right
     :next-angles [down-left down-right]
     ;; if left distance is lower (i.e. left is upwards), go there.
     :dir (if (and left right)
            (if (< left right) -1 1)
            ;; return dir 0 if raycasts not completed yet
            0)}))
