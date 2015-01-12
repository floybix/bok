(ns org.nfrac.hatto.visual-runner
  (:require [org.nfrac.hatto.runner :as runner]
            [org.nfrac.hatto.core :as core]
            [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer [step!]]
            [quil.core :as quil]
            [quil.middleware]
            [zeromq.zmq :as zmq]))

(defn run-with-display
  [game]
  (let [p (promise)]
    (quil/sketch
     :title "Hatto"
     :setup (fn [] (merge bed/initial-state game))
     :update runner/step
     :draw bed/draw
     :key-typed bed/key-press
     :mouse-pressed bed/mouse-pressed
     :mouse-released bed/mouse-released
     :mouse-dragged bed/mouse-dragged
     :size [1200 600]
     :middleware [quil.middleware/fun-mode]
     :on-close (fn [state] (deliver p state)))
    @p))

(defn -main
  [addr-a addr-b & [arena-type more-args]]
  (let [ctx (zmq/context 1)
        arena-type (keyword (or arena-type "simple"))]
    (println "connecting to" addr-a)
    (println "connecting to" addr-b)
    (with-open [sock-a (doto (zmq/socket ctx :req)
                         (zmq/connect addr-a))
                sock-b (doto (zmq/socket ctx :req)
                         (zmq/connect addr-b))]
      (println "connected.")
      (-> (runner/start-bout arena-type sock-a sock-b)
          (run-with-display)
          (runner/end-bout)
          (println)))))
