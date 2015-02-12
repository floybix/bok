(ns org.nfrac.bok.visual-runner
  (:require [org.nfrac.bok.runner :as runner :refer [PLAYER_KEYS]]
            [org.nfrac.bok.entities :as ent]
            [org.nfrac.cljbox2d.core :refer [position center angle user-data
                                             body-a body-b anchor-a radius
                                             fixture-of vary-user-data]]
            [org.nfrac.cljbox2d.vec2d :refer [v-dist v-add polar-xy v-scale
                                              v-sub v-angle v-mag]]
            [org.nfrac.cljbox2d.testbed :as bed]
            [quil.core :as quil]
            [quil.middleware])
  (:import [org.zeromq ZMQ ZMQ$Context ZMQ$Socket ZMQException]))

(defn mean
  [xs]
  (/ (reduce + xs)
     (count xs)))

(defn colors*
  []
  {:background (quil/color 200 200 160)
   :text (quil/color 0 0 0)
   :static (quil/color 128 0 0)
   :kinematic (quil/color 128 0 0)
   :dynamic (quil/color 128 64 0)
   :joint (quil/color 64 0 64)
   :poi (quil/color 255 255 255)
   :com (quil/color 0 255 0)
   :gun (quil/color 64 0 32)
   :winner (quil/color 100 100 200)
   :loser (quil/color 32 32 32)})

(defn draw-additional
  [scene camera colors]
  (let [->px (bed/world-to-px-fn camera)
        px-scale (bed/world-to-px-scale camera)]
    ;; guns
    (quil/stroke (:gun colors))
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
              :let [color (if (= player-key winner)
                            (:winner colors)
                            (:loser colors))
                    bodies (vals (get-in scene [:bodies player-key]))]]
        (quil/stroke 255)
        (quil/fill color)
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
  [game detail-level colors]
  (let [camera (:camera game)
        ->px (bed/world-to-px-fn camera)
        px-scale (bed/world-to-px-scale camera)]
    ;; details, labels
    (quil/fill (:text colors))
    (when (zero? detail-level)
      (quil/text "Press d to show details." 10 10))
    (when (pos? detail-level)
      (doseq [[ent-key ent] (:entities game)
              :let [{:keys [components joints]} ent
                    simple-ent? (== (count components) 1)
                    [labx laby] (->px (label-pt ent))
                    com (ent/entity-center-of-mass ent)
                    comv (ent/entity-velocity ent)]]
        ;; when detail-level = 1, only label the players
        (when (or (contains? (:player-keys game) ent-key)
                  (> detail-level 1))
          ;; label the entity
          (quil/text-align :center)
          (quil/fill (:text colors))
          (quil/text (name ent-key)
                     labx laby))
        ;; label all components and draw points of interest
        (when (<= 2 detail-level 3)
          (doseq [[cmp-key body] components]
            ;; draw the points of interest
            (quil/stroke (:text colors))
            (quil/fill (:poi colors))
            (doseq [pt (:points-of-interest (user-data body))
                    :let [[x y] (->px (position body pt))
                          r-px (* px-scale 0.1)]]
              (quil/ellipse x y r-px r-px))
            (when-not simple-ent?
              ;; label the components
              (quil/fill (:text colors))
              (quil/with-translation (->px (center body))
                (quil/with-rotation [(- (angle body))]
                  (quil/text (name cmp-key)
                             0 0))))))
        ;; center of mass
        (when (= 3 detail-level)
          (quil/stroke (:com colors))
          (quil/with-translation (->px com)
            (quil/with-rotation [(- (v-angle comv))]
              (let [z (* px-scale (v-mag comv))]
                (quil/line [0 0] [z 0])
                (quil/line [z 0] [(- z 3) 2])
                (quil/line [z 0] [(- z 3) -2])))))
        ;; joints
        (when (= 4 detail-level)
          (quil/text-align :right :center)
          ;; label the joints
          (doseq [[jt-key jt] joints
                  :let [anch (anchor-a jt)
                        body-a (body-a jt)
                        body-b (body-b jt)
                        ang (v-angle (v-sub (center body-b) anch))
                        radius-px (* 2 px-scale (v-dist (center body-b) anch))]]
            (quil/stroke (:joint colors))
            (quil/fill (:joint colors) 64)
            (let [[x y] (->px anch)]
              (quil/arc x y (* 2 radius-px) (* 2 radius-px)
                        (- 0 ang 0.5)
                        (- 0 ang)))
            (quil/fill (:text colors))
            (quil/with-translation (->px anch)
              (quil/with-rotation [(- 0 ang 0.25)]
                (quil/text (name jt-key)
                           (* 0.95 radius-px) 0))))
          (quil/text-align :left :baseline))))))

(defn draw
  [game]
  (let [colors (colors*)]
    ;; standard drawing
    (bed/draw (assoc game ::bed/colors colors))
    (let [{:keys [world snapshots steps-back time camera]} game
          scene (nth snapshots steps-back nil)
          detail-level (::detail-level game 0)]
      (draw-additional scene camera colors)
      (draw-details game detail-level colors))))

(defn key-press
  "Standard actions for key events"
  [state event]
  (case (:raw-key event)
    \d (update-in state [::detail-level] (fn [i] (-> (inc (or i 0))
                                                    (mod 5))))
    \q (do
         (quil/exit)
         (assoc state :error "User quit."))
    (bed/key-press state event)))

(defn record-snapshot
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
    game
    (let [game (-> (step game)
                   (record-snapshot)
                   (assoc :stepping? false
                          :paused? (:stepping? game)))]
      (if-let [res (:final-result game)]
        ;; game has already ended
        (if (> (:time game)
               (+ CONTINUE_SECS (:end-time res)))
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
  [game step]
  (let [p (promise)]
    (quil/sketch
     :title "Bok"
     :setup (fn []
              (quil/frame-rate (/ (:dt-secs game)))
              (merge bed/initial-state game))
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
    @p))

(defn main
  [^ZMQ$Context ctx arena-type addrs opts]
  (runner/with-all-connected ctx ZMQ/REQ addrs
    (fn [socks]
      (-> (runner/start-bout arena-type (zipmap PLAYER_KEYS socks) opts)
          (run-with-display runner/step-remote)
          (runner/end-bout)))))

(defn -main
  [arena-type & addrs]
  (let [ctx (ZMQ/context 1)
        arena-type (keyword arena-type)]
    (assert (pos? (count addrs)))
    (println "connecting to" addrs)
    (try
      (-> (main ctx arena-type addrs {})
          :final-result
          (println))
      (finally
        (println "closing ZMQ context")
        (.term ctx)))))

