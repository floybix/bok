(ns org.nfrac.hatto.runner
  (:require [org.nfrac.hatto.core :as core]
            [org.nfrac.hatto.arena-simple :as arenas]
            [cljbox2d.core :refer [step!]]
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
    (let [{:keys [bout-id sock-a sock-b]} state
          obs-a (core/perceive state :creature-a)
          obs-b (core/perceive state :creature-b)]
      (send-msg sock-a {:type :react, :bout-id bout-id, :data obs-a})
      (send-msg sock-b {:type :react, :bout-id bout-id, :data obs-b})
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

(defn run-bout
  [game]
  (loop [game game]
    (if (:finished? game)
      game
      (recur (step game)))))

(defn end-bout
  [game]
  (let [{:keys [bout-id sock-a sock-b]} state]
    (send-msg sock-a {:type :finished, :bout-id bout-id})
    (send-msg sock-b {:type :finished, :bout-id bout-id})
    (assert (= :bye (:type (recv-msg sock-a))))
    (assert (= :bye (:type (recv-msg sock-b))))
    game))

(defn start-bout
  [sock-a sock-b]
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
      (-> (core/setup-game (:creature-type ident-a)
                           (:creature-type ident-b))
          (assoc :bout-id bout-id
                 :ident-a ident-a
                 :ident-b ident-b
                 :sock-a sock-a
                 :sock-b sock-b)))))

(defn -main
  [addr-a addr-b & more-args]
  (let [ctx (zmq/context 1)]
    (println "connecting to" addr-a)
    (println "connecting to" addr-b)
    (let ;with-open
        [sock-a (doto (zmq/socket ctx :req)
                  (zmq/connect addr-a))
         sock-b (doto (zmq/socket ctx :req)
                  (zmq/connect addr-b))
         ]
      (println "connected.")
      (-> (start-bout sock-a sock-b)
          (run-bout)
          (end-bout)
          (println)))))
