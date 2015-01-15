(ns org.nfrac.hatto.runner
  (:require [org.nfrac.hatto.core :as core]
            [cljbox2d.core :refer [step!]]
            [cognitect.transit :as transit])
  (:import [org.zeromq ZMQ ZMQ$Context ZMQ$Socket ZMQException]
           [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn to-transit
  ^bytes [data]
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
  [^ZMQ$Socket socket]
  (from-transit
   (or (.recv socket)
       (throw (ZMQException. 0))))) ;; TODO

(defn send-msg
  [^ZMQ$Socket socket msg]
  (or (.send socket (to-transit msg))
      (throw (ZMQException. 0)))) ;; TODO

(defn take-remote-actions
  [game]
  (if (core/act-now? game)
    (let [{:keys [bout-id sock-a sock-b]} game
          obs-a (core/perceive game :creature-a)
          obs-b (core/perceive game :creature-b)]
      (send-msg sock-a {:type :react, :bout-id bout-id, :data obs-a})
      (send-msg sock-b {:type :react, :bout-id bout-id, :data obs-b})
      (doseq [[player-key sock] [[:creature-a sock-a]
                                 [:creature-b sock-b]]]
        (let [msg (recv-msg sock)
              actions (:data msg)]
          (core/act! (get-in game [:entities player-key]) actions)))
      (assoc game :last-act-time (:time game)))
    game))

(defn step-remote
  [game]
  (try
    (-> game
        (update-in [:world] step! (:dt-secs game))
        (update-in [:time] + (:dt-secs game))
        (take-remote-actions))
    (catch ZMQException e
      (assoc game :error (str e)))))

(defn run-bout
  [game]
  (loop [game game]
    (if-let [res (core/final-result game)]
      (assoc game :final-result res)
      (recur (step-remote game)))))

(defn end-bout
  [game]
  (let [{:keys [bout-id sock-a sock-b]} game
        result (:final-result game)]
    (if (:error result)
      (do
        ;; something
        )
      (let [winner (:winner result)
            msg {:type :finished, :bout-id bout-id
                 :winner winner}]
        (send-msg sock-a msg)
        (send-msg sock-b msg)
        (assert (= :bye (:type (recv-msg sock-a))))
        (assert (= :bye (:type (recv-msg sock-b))))))
    game))

(defn start-bout
  [arena-type sock-a sock-b]
  (let [bout-id (str (java.util.UUID/randomUUID))]
    (println "inviting to bout" bout-id)
    (send-msg sock-a {:type :invite, :bout-id bout-id})
    (send-msg sock-b {:type :invite, :bout-id bout-id})
    (assert (= :join (:type (recv-msg sock-a))))
    (assert (= :join (:type (recv-msg sock-b))))
    (doseq [^ZMQ$Socket socket [sock-a sock-b]]
      (.setSendTimeOut socket 1000)
      (.setReceiveTimeOut socket 1000))
    (println "identifying players.")
    (send-msg sock-a {:type :identify})
    (send-msg sock-b {:type :identify})
    (let [ident-a (recv-msg sock-a)
          ident-b (recv-msg sock-b)]
      (println ident-a)
      (println ident-b)
      (-> (core/setup-game arena-type
                           (:creature-type ident-a)
                           (:creature-type ident-b))
          (assoc :gameover-secs 60
                 :bout-id bout-id
                 :ident-a ident-a
                 :ident-b ident-b
                 :sock-a sock-a
                 :sock-b sock-b)))))

(defn main
  [^ZMQ$Context ctx addr-a addr-b arena-type]
  (with-open [sock-a (.socket ctx ZMQ/REQ)
              sock-b (.socket ctx ZMQ/REQ)]
    (.connect sock-a addr-a)
    (.connect sock-b addr-b)
    (-> (start-bout arena-type sock-a sock-b)
        (run-bout)
        (end-bout))))

(defn -main
  [addr-a addr-b & [arena-type more-args]]
  (let [ctx (ZMQ/context 1)
        arena-type (keyword (or arena-type "simple"))]
    (println "connecting to" addr-a)
    (println "connecting to" addr-b)
    (try
      (-> (main ctx addr-a addr-b arena-type)
          :final-result
          (println))
      (finally
        (println "closing ZMQ context")
        (.term ctx)))))
