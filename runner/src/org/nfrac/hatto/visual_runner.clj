(ns org.nfrac.hatto.visual-runner
  (:require [org.nfrac.hatto.runner :as runner :refer [PLAYER_KEYS]]
            [org.nfrac.cljbox2d.core :refer [position center angle user-data
                                             body-a body-b anchor-a radius
                                             fixture-of vary-user-data]]
            [org.nfrac.cljbox2d.vec2d :refer [v-dist v-add polar-xy]]
            [org.nfrac.cljbox2d.testbed :as bed]
            [quil.core :as quil]
            [quil.middleware])
  (:import [org.zeromq ZMQ ZMQ$Context ZMQ$Socket ZMQException]))

(defn mean
  [xs]
  (/ (reduce + xs)
     (count xs)))

(defn hatto-draw
  [game]
  ;; if game over, highlight winner vs others
  (when-let [winner (:winner (:final-result game))]
    (doseq [player-key (:player-keys game)
            :let [rgb (if (= player-key winner)
                        [255 255 255]
                        [128 64 64])
                  ent (get-in game [:entities player-key])]]
      (doseq [body (vals (:components ent))]
        (vary-user-data body #(assoc % ::bed/rgb rgb)))))
  ;; standard drawing
  (bed/draw game)
  (let [cam (:camera game)
        ->px (partial bed/world-to-px cam)
        px-scale (bed/world-to-px-scale cam)]
    ;; guns
    (quil/stroke-weight 2)
    (doseq [[player-key gun-info] (:player-gun game)
            :let [head (get-in game [:entities player-key :components :head])
                  gun-length (* 2 (radius (fixture-of head)))
                  gun-length-px (* px-scale gun-length)]]
      (quil/stroke 200)
      (quil/with-translation (->px (position head))
        (quil/with-rotation [(- (:angle gun-info))]
          (quil/line [0 2] [gun-length-px 2])
          (quil/line [0 -2] [gun-length-px -2]))))
    (quil/stroke-weight 1)
    ;; details, labels
    (quil/fill 255)
    (quil/text-align :left)
    (when-not (:show-details? game)
      (quil/text "Press ? for details." 10 10))
    (when (:show-details? game)
      (quil/text (str "Drag bodies to move them. Right-button drag to pan. +/- to zoom.\n"
                      "Press space to pause. Press q to quit.\n"
                      "Press d to switch display detail.")
                 10 10)
      (doseq [[ent-key ent] (:entities game)
              :let [{:keys [components joints]} ent
                    mid-x (mean (map (comp first position)
                                     (vals components)))
                    top (apply max (map (comp second position)
                                        (vals components)))
                    [labx laby] (->px [mid-x (+ top 1)])]]
        (quil/text-align :center :top)
        (quil/text (name ent-key)
                   labx laby)
        (when (= 0 (:show-details-level game 0))
          (quil/text-align :center :top)
          (quil/stroke (quil/color 255 0 0))
          (doseq [[cmp-key body] components]
            (doseq [pt (:points-of-interest (user-data body))
                    :let [[x y] (->px (position body pt))]]
              (quil/ellipse x y 10 10))
            (quil/with-translation (->px (center body))
              (quil/with-rotation [(- (angle body))]
                (quil/text (name cmp-key)
                           0 0)))))
        (when (= 1 (:show-details-level game 0))
          (quil/text-align :right :center)
          (doseq [[jt-key jt] joints
                  :let [anch (anchor-a jt)
                        body-a (body-a jt)
                        body-b (body-b jt)
                        radius-px (* 2 px-scale (v-dist anch
                                                        (center body-b)))]]
            (quil/stroke (quil/color 255 255 0))
            (quil/fill (quil/color 255 255 0) 64)
            (apply quil/arc (concat (->px anch)
                                    (repeat 2 (* 2 radius-px))
                                    [(- 0 (angle body-b) 0.5)
                                     (- (angle body-b))]))
            (quil/fill 255)
            (quil/with-translation (->px anch)
              (quil/with-rotation [(- 0 (angle body-b) 0.25)]
                (quil/text (name jt-key)
                           (* 0.95 radius-px) 0)))))))))

(defn key-press
  "Standard actions for key events"
  [state event]
  (case (:raw-key event)
    (\/ \?) (update-in state [:show-details?] not)
    \d (update-in state [:show-details-level] (fn [i] (-> (inc (or i 0))
                                                         (mod 2))))
    \q (do
         (quil/exit)
         (assoc state :error "User quit."))
    (bed/key-press state event)))

(def CONTINUE_SECS 4)

(defn gui-step
  [game step]
  (if (:paused? game)
    game
    (let [game (step game)]
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
     :title "Hatto"
     :setup (fn []
              (quil/frame-rate 30)
              (merge bed/initial-state game))
     :update (fn [game]
               (gui-step game step))
     :draw hatto-draw
     :key-typed key-press
     :mouse-pressed bed/mouse-pressed
     :mouse-released bed/mouse-released
     :mouse-dragged bed/mouse-dragged
     :size [1200 600]
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

