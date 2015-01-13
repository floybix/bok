(ns org.nfrac.hatto.visual-runner
  (:require [org.nfrac.hatto.runner :as runner]
            [org.nfrac.hatto.core :as core]
            [org.nfrac.cljbox2d.testbed :as bed]
            [quil.core :as quil]
            [quil.middleware]
            [zeromq.zmq :as zmq]))

(defn step-remote
  [game]
  (if (:paused? game)
    game
    (if-let [res (core/final-result game)]
      (do
        (quil/exit)
        (assoc game :final-result res))
      (runner/step-remote game))))

(defn run-with-display
  [game step]
  (let [p (promise)]
    (quil/sketch
     :title "Hatto"
     :setup (fn []
              (quil/frame-rate 30)
              (merge bed/initial-state game))
     :update step
     :draw bed/draw
     :key-typed bed/key-press
     :mouse-pressed bed/mouse-pressed
     :mouse-released bed/mouse-released
     :mouse-dragged bed/mouse-dragged
     :size [1200 600]
     :middleware [quil.middleware/fun-mode]
     :on-close (fn [state] (deliver p state)))
    @p))

(defn main
  [ctx addr-a addr-b arena-type]
  (with-open [sock-a (doto (zmq/socket ctx :req)
                       (zmq/connect addr-a))
              sock-b (doto (zmq/socket ctx :req)
                       (zmq/connect addr-b))]
    (-> (runner/start-bout arena-type sock-a sock-b)
        (run-with-display step-remote)
        (runner/end-bout)
        (println))))

(defn -main
  [addr-a addr-b & [arena-type more-args]]
  (let [ctx (zmq/context 1)
        arena-type (keyword (or arena-type "simple"))]
    (println "connecting to" addr-a)
    (println "connecting to" addr-b)
    (main ctx addr-a addr-b arena-type)))
