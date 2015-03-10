(ns org.nfrac.bok.visual-runner
  (:require [org.nfrac.bok.runner :as runner :refer [PLAYER_KEYS]]
            [org.nfrac.bok.entities :as ent]
            [org.nfrac.cljbox2d.core :refer [position center angle user-data
                                             body-a body-b anchor-a radius]]
            [org.nfrac.cljbox2d.vec2d :refer [v-dist v-add polar-xy v-scale
                                              v-sub v-angle v-mag abs]]
            [org.nfrac.cljbox2d.testbed :as bed]
            [quil.core :as quil :refer [color fill stroke]]
            [quil.middleware]
            [org.nfrac.bok.tree-diff :as diff]
            [clojure.java.io :as io])
  (:import [org.zeromq ZMQ ZMQ$Context ZMQ$Socket ZMQException]))

(defn mean
  [xs]
  (/ (reduce + xs)
     (count xs)))

(defn styler
  ([k]
     (case k
       :text (fill (color 0 0 0))
       :joint (fill (color 64 0 64))
       :background (do (fill (color 200 200 160))
                       (quil/no-stroke))
       :poi (do (fill (color 255 255 255))
                (stroke (color 0 0 0)))
       :com (stroke (color 0 255 0))
       :gun (stroke (color 64 0 32))
       :raycast (stroke (color 100 100 100) 64)
       :winner (do (fill (color 200 200 200))
                   (stroke (color 0 0 0)))
       :loser (do (fill (color 100 100 100))
                  (stroke (color 0 0 0)))))
  ([type ud]
     (cond
      (:vortex? ud) (do (fill (color 64 0 64))
                        (quil/no-stroke))
      :else
      (case type
        :static (do (fill (unchecked-int 0xFFa46450))
                    (stroke (color 128 0 0)))
        :kinematic (do (fill (color 128 64 0) 128)
                       (stroke (color 128 64 0)))
        :dynamic (do (fill (unchecked-int 0xFF997b4b))
                     (stroke (color 128 64 0)))))))

(defn draw-additional
  [scene camera style!]
  (let [->px (bed/world-to-px-fn camera)
        px-scale (bed/world-to-px-scale camera)]
    ;; raycasts
    (style! :raycast)
    (doseq [[player-key rcs] (:player-raycast scene)
            :let [head (get-in scene [:bodies player-key :head])]
            rc rcs
            :let [rc-length-px (* px-scale (or (:distance rc) 50.0))]]
      (quil/with-translation (->px (:position head))
        (quil/with-rotation [(- (:angle rc))]
          ;; dashed line
          (doseq [i (range 0 (quot rc-length-px 8) 2)]
            (quil/line [(* i 8) 0] [(* (inc i) 8) 0])))))
    ;; guns
    (style! :gun)
    (quil/stroke-weight 2)
    (doseq [[player-key gun-info] (:player-gun scene)
            :let [head (get-in scene [:bodies player-key :head])
                  gun-length (* 2 (:radius (first (:fixtures head))))
                  gun-length-px (* px-scale gun-length)]]
      (quil/with-translation (->px (:position head))
        (quil/with-rotation [(- (:angle gun-info))]
          (quil/line [0 2] [gun-length-px 2])
          (quil/line [0 -2] [gun-length-px -2]))))
    (quil/stroke-weight 1)
    ;; energy
    (doseq [[player-key energy] (:player-energy scene)
            :let [head (get-in scene [:bodies player-key :head])
                  r (:radius (first (:fixtures head)))
                  r-px (* px-scale r)
                  FULL 300.0]]
      (quil/fill (quil/color (if (> energy FULL) (min 255 (- energy FULL)) 0)
                             200
                             (if (> energy FULL) (min 255 (- energy FULL)) 0))
                 128)
      (quil/with-translation (->px (:position head))
        (quil/arc 0 0 (* r-px 2) (* r-px 2)
                  0 (* 2 Math/PI (min FULL energy) (/ FULL)))))
    ;; if game over, highlight winner vs others
    (when-let [winner (:winner (:final-result scene))]
      (doseq [player-key (:player-keys scene)
              :let [bodies (vals (get-in scene [:bodies player-key]))]]
        (style! (if (= player-key winner) :winner :loser))
        (doseq [body-snap bodies]
          (bed/draw-body body-snap ->px px-scale))))))

(defn label-pt
  [ent]
  (let [components (:components ent)]
    (if (== (count components) 1)
      (let [body (ent/entity-body ent)]
        (if-let [pois (->> body
                           (user-data)
                           :points-of-interest
                           (map #(position body %))
                           (seq))]
          (v-scale (reduce v-add pois)
                   (/ (count pois)))
          (center (ent/entity-body ent))))
      (let [pts (map position (vals components))]
        [(mean (map first pts))
         (+ 1 (apply max (map second pts)))]))))

(defn draw-details
  [game detail-level style!]
  (let [camera (:camera game)
        ->px (bed/world-to-px-fn camera)
        px-scale (bed/world-to-px-scale camera)]
    ;; details, labels
    (style! :text)
    (when (zero? detail-level)
      (quil/text (str "Press d to show details, f to follow players, "
                      "! to run fast.") 10 10))
    (when (pos? detail-level)
      (doseq [[ent-key ent] (:entities game)
              :let [{:keys [components joints]} ent
                    simple-ent? (== (count components) 1)
                    [labx laby] (->px (label-pt ent))
                    com (ent/entity-center-of-mass ent)
                    comv (ent/entity-velocity ent)]]
        ;; when detail-level = 1, only label the player entities
        ;; when detail-level >= 2, label all entities
        (when (or (contains? (:player-keys game) ent-key)
                  (> detail-level 1))
          ;; label the entity
          (quil/text-align :center)
          (style! :text)
          (quil/text (name ent-key)
                     labx laby))
        ;; when detail-level in [3-5], draw all points of interest
        ;; when detail-level = 3, label non-player components
        ;; when detail-level = 4, label all components
        ;; when detail-level = 5, label all components + draw COM
        (when (<= 3 detail-level 5)
          (doseq [[cmp-key body] components]
            ;; draw the points of interest
            (style! :poi)
            (doseq [pt (:points-of-interest (user-data body))
                    :let [[x y] (->px (position body pt))
                          r-px (* px-scale 0.1)]]
              (quil/ellipse x y r-px r-px))
            (when (not simple-ent?)
              (when (or (not (contains? (:player-keys game) ent-key))
                        (>= detail-level 4))
                ;; label the components
                (style! :text)
                (quil/with-translation (->px (center body))
                  (quil/with-rotation [(- (angle body))]
                    (quil/text (name cmp-key)
                               0 0)))))))
        ;; center of mass
        (when (= 5 detail-level)
          (style! :com)
          (quil/with-translation (->px com)
            (quil/with-rotation [(- (v-angle comv))]
              (let [z (* px-scale (v-mag comv))]
                (quil/line [0 0] [z 0])
                (quil/line [z 0] [(- z 3) 2])
                (quil/line [z 0] [(- z 3) -2])))))
        ;; when detail-level = 5, draw joints
        (when (= 6 detail-level)
          (quil/text-align :right :center)
          ;; label the joints
          (doseq [[jt-key jt] joints
                  :let [anch (anchor-a jt)
                        body-a (body-a jt)
                        body-b (body-b jt)
                        radius (v-dist (center body-b) anch)
                        radius-px (* 2 px-scale radius)
                        ang (if (< radius 0.02)
                              (angle body-b)
                              (v-angle (v-sub (center body-b) anch)))]]
            (style! :joint)
            (quil/fill (quil/current-stroke) 64)
            (let [[x y] (->px anch)]
              (quil/arc x y (* 2 radius-px) (* 2 radius-px)
                        (- 0 ang 0.5)
                        (- 0 ang)))
            (style! :text)
            (quil/with-translation (->px anch)
              (quil/with-rotation [(- 0 ang 0.25)]
                (quil/text (name jt-key)
                           (* 0.95 radius-px) 0))))
          (quil/text-align :left :baseline))))))

(defn draw
  [game]
  (when (or (not (::fast? game))
            (< (mod (:time game) 3.0) (:dt-secs game)))
    ;; standard drawing
    (bed/draw (assoc game ::bed/styler styler))
    (let [{:keys [world snapshots steps-back time camera]} game
          scene (nth snapshots steps-back nil)
          detail-level (::detail-level game 0)]
      (draw-additional scene camera styler)
      (draw-details game detail-level styler))))

(defn key-press
  "Standard actions for key events"
  [state event]
  (case (:raw-key event)
    \d (update-in state [::detail-level] (fn [i] (-> (inc (or i 0))
                                                    (mod 7))))
    \f (update-in state [::follow-idx]
                  (fn [i] (-> (inc (or i 0))
                             (mod (inc (count (:player-keys state)))))))
    \! (if (::fast? state)
         (do
           (quil/frame-rate (/ (:dt-secs state)))
           (dissoc state ::fast?))
         (do
           (quil/frame-rate 300)
           (assoc state ::fast? true)))
    \q (assoc state :error "User quit" :paused? false)
    (bed/key-press state event)))

(defn camera-follow
  [state]
  (let [idx (::follow-idx state 0)]
    (if (zero? idx)
      state
      (let [follow-player (nth (seq (:player-keys state))
                               (dec idx))
            [px py] (-> state :current-scene :bodies follow-player :head :position)
            {:keys [width height]} (:camera state)]
        (update-in state [:camera :center]
                   (fn [[x y]]
                     (let [dx (- px x)
                           dy (- py y)
                           sign #(if (neg? %) -1 1)
                           ;; offsets in metres exceeding tolerance margin (1/4)
                           ox (-> (- (abs dx) (/ width 4))
                                  (max 0)
                                  (* (sign dx)))
                           oy (-> (- (abs dy) (/ height 4))
                                  (max 0)
                                  (* (sign dy)))]
                       ;; adjust at faster % for larger offsets (don't lose it!)
                       [(+ x (* ox (/ (abs ox) width)))
                        (+ y (* oy (/ (abs oy) height)))])))))))

(defn record-snapshot
  "Allows rewind"
  [game]
  (let [keep-n (:keep-snapshots game)
        ;; runner has already instantiated the current scene
        scene (:current-scene game)]
    (cond-> (update-in game [:snapshots] conj scene)
            ;; limit size of history buffer
            (>= (count (:snapshots game)) keep-n)
            (update-in [:snapshots] (partial take (* 0.9 keep-n))))))

(def CONTINUE_SECS 4)

(defn gui-step
  [game step]
  (if (:paused? game)
    (camera-follow game)
    (let [game (-> (step game)
                   (camera-follow)
                   (record-snapshot)
                   (assoc :stepping? false
                          :paused? (:stepping? game)))]
      (if-let [res (:final-result game)]
        ;; game has already ended
        (if (or (> (:time game)
                   (+ CONTINUE_SECS (:end-time res)))
                (:error res))
          ;; enough, shut it down
          (do
            (quil/exit)
            game)
          ;; we are continuing with :dead-players for visual effect
          game)
        ;; game has not ended yet
        (if-let [res (runner/final-result game)]
          (assoc game :final-result res)
          game)))))

(defn run-with-display
  ([game step]
     (run-with-display game step false))
  ([game step fast?]
     (let [p (promise)]
       (quil/sketch
        :title "Bok"
        :setup (fn []
                 (quil/frame-rate (if fast? 300 (/ (:dt-secs game))))
                 (merge bed/initial-state game {::fast? fast?}))
        :update (fn [game]
                  (gui-step game step))
        :draw draw
        :key-typed key-press
        :mouse-pressed bed/mouse-pressed
        :mouse-released bed/mouse-released
        :mouse-dragged bed/mouse-dragged
        :mouse-wheel bed/mouse-wheel
        :size [1200 600]
        :features [:resizable]
        :middleware [quil.middleware/fun-mode]
        :on-close (fn [state] (deliver p state)))
       @p)))

(defn main
  [^ZMQ$Context ctx game-id addrs opts]
  (runner/with-all-connected ctx ZMQ/REQ addrs
    (fn [socks]
      (let [b (->
               (runner/start-bout game-id (zipmap PLAYER_KEYS socks) opts)
               (run-with-display runner/step-remote (:fast-vis opts))
               (runner/end-bout))
            res (:final-result b)]
        (println res)
        (when (and (:repeat opts) (not (:error res)))
          (recur socks))))))

(defn replay-game
  [saved-game-data]
  (let [[game-meta deltas] saved-game-data
        dt-secs (:dt-secs (first deltas))
        init-state (-> (assoc bed/initial-state
                         :current-scene (first deltas)
                         :more-deltas (next deltas)
                         :dt-secs dt-secs
                         :player-keys (:player-keys (first deltas)))
                       (record-snapshot))
        step (fn [state]
               (-> state
                   ;; will be picked up by record-snapshot
                   (update-in [:current-scene] diff/patch
                              (first (:more-deltas state)))
                   (update-in [:more-deltas] next)
                   (update-in [:time] + dt-secs)))]
    (println game-meta)
    (quil/sketch
     :title "Bok (replay)"
     :setup (fn []
              (quil/frame-rate (/ dt-secs))
              init-state)
     :update (fn [game]
               (if (:paused? game)
                 (camera-follow game)
                 (let [game (-> (step game)
                                (camera-follow)
                                (record-snapshot)
                                (assoc :stepping? false
                                       :paused? (:stepping? game)))]
                   (when-not (:more-deltas game)
                     (quil/exit))
                   game)))
     :draw draw
     :key-typed key-press
     :mouse-pressed bed/mouse-pressed
     :mouse-released bed/mouse-released
     :mouse-dragged bed/mouse-dragged
     :mouse-wheel bed/mouse-wheel
     :size [1200 600]
     :features [:resizable]
     :middleware [quil.middleware/fun-mode]
     )))

(defn replay-game-from-file
  [file]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (io/copy (io/file file) out)
    (replay-game (runner/from-transit (.toByteArray out)))))
