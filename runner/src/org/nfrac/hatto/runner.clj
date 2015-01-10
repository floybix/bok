(ns org.nfrac.hatto.runner
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.hatto.arena-simple :as arenas]
            [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer [step!]]
            [quil.core :as quil]
            [quil.middleware]
            [zeromq.zmq :as zmq]
            [cognitect.transit :as transit])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn to-transit
  [data]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :json)]
    (transit/write writer data)
    (.toByteArray out)))

(defn from-transit
  [bytes]
  (let [in (ByteArrayInputStream. bytes)
        reader (transit/reader in :json)]
    (transit/read reader)))

(defn recv-msg
  [socket]
  (from-transit (zmq/receive socket)))

(defn send-msg
  [socket msg]
  (zmq/send socket (to-transit msg)))

(defn take-remote-actions
  [state]
  (if (core/act-now? state)
    (let [{:keys [sock-a sock-b]} state
          obs-a (core/perceive state :creature-a)
          obs-b (core/perceive state :creature-b)]
      (send-msg sock-a {:type :react :data obs-a})
      (send-msg sock-b {:type :react :data obs-b})
      (let [act-a (:data (recv-msg sock-a))
            act-b (:data (recv-msg sock-b))]
        (core/act! (:creature-a (:entities state)) act-a)
        (core/act! (:creature-b (:entities state)) act-b)
        (assoc state :last-act-time (:time state))))
    state))

(defn step
  [state]
  (if (:paused? state)
    state
    (-> state
        (update-in [:world] step! (:dt-secs state))
        (update-in [:time] + (:dt-secs state))
        (take-remote-actions))))

(defn run-with-display
  [game]
  (quil/defsketch bout-sketch
    :title "Hatto"
    :setup (fn [] (merge bed/initial-state game))
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [1200 600]
    :middleware [quil.middleware/fun-mode])
  )

(defn run-bout
  [sock-a sock-b & {:keys [display?]}]
  (let [bout-id (str (java.util.UUID/randomUUID))]
    (println "inviting to bout" bout-id)
    (send-msg sock-a {:type :invite, :bout-id bout-id})
    (send-msg sock-b {:type :invite, :bout-id bout-id})
    (assert (= :join (:type (recv-msg sock-a))))
    (assert (= :join (:type (recv-msg sock-b))))
    (println "identifying players.")
    (send-msg sock-a {:type :identify})
    (send-msg sock-b {:type :identify})
    (let [ident-a (recv-msg sock-a)
          ident-b (recv-msg sock-b)]
      (println ident-a)
      (println ident-b)
      (let [game (-> (core/setup-game (:creature-type ident-a)
                                      (:creature-type ident-b))
                     (assoc :sock-a sock-a
                            :sock-b sock-b))]
        (run-with-display game)
        ))))

(defn -main
  [addr-a addr-b & more-args]
  (let [ctx (zmq/context 1)]
    (println "connecting to" addr-a)
    (println "connecting to" addr-b)
    (let ;with-open
        [sock-a (doto (zmq/socket ctx :req)
                  (zmq/connect addr-a))
         sock-b (doto (zmq/socket ctx :req)
                  (zmq/connect addr-b))]
      (println "connected.")
      (run-bout sock-a sock-b))))
