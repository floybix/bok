(ns org.nfrac.bok.cljplayer
  (:require [cognitect.transit :as transit])
  (:import [org.zeromq ZMQ ZMQ$Context ZMQ$Socket ZMQException]
           [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn to-transit
  ^bytes [data]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :msgpack)]
    (transit/write writer data)
    (.toByteArray out)))

(defn from-transit
  [bytes]
  (let [in (ByteArrayInputStream. bytes)
        reader (transit/reader in :msgpack)]
    (transit/read reader)))

(defn recv-msg
  [^ZMQ$Socket socket]
  (from-transit
   (or (.recv socket)
       (throw (Exception. "ZMQ recv failed.")))))

(defn send-msg
  [^ZMQ$Socket socket msg]
  (or (.send socket (to-transit msg))
      (throw (Exception. "ZMQ send failed."))))

(defn patch
  "Applies a `diff` map to the original map `m`. Like merge, but
   merges maps recursively."
  [m diff]
  (if (and (map? m) (map? diff))
    (merge-with patch m diff)
    diff))

(defn run-one-bout
  "Returns the final result of the bout. The last scene is also included
  under key :last-scene."
  [socket ident action-fn peek-ref]
  (loop [bouts @peek-ref]
    (let [msg (recv-msg socket)]
      (reset! peek-ref bouts)
      (case (:type msg)
        :identify (do
                    (send-msg socket {:type :ident
                                      :data ident})
                    (recur bouts))
        :invite (let [bout-id (:bout-id msg)]
                  (send-msg socket {:type :join
                                    :bout-id bout-id})
                  (recur (assoc bouts bout-id {})))
        :react (let [bout-id (:bout-id msg)
                     last-state (bouts bout-id)
                     state (-> last-state
                               (update-in [:current]
                                          patch (:data msg))
                               (action-fn))
                     actions (:actions state)]
                 (send-msg socket {:type :actions
                                   :data actions})
                 (recur (assoc bouts bout-id state)))
        :final-result (let [bout-id (:bout-id msg)]
                        (send-msg socket {:type :bye})
                        (swap! peek-ref dissoc bout-id)
                        (-> (:data msg)
                            (assoc :last-scene (bouts bout-id))))
        (println "Unrecognised message type:" msg)))))

(defn run-server
  "Receives messages on the socket in an infinite loop. The atom
   `peek-ref` stores state for any active bouts, in a map keyed by
   bout id."
  [socket ident action-fn peek-ref]
  (loop []
    (run-one-bout socket ident action-fn peek-ref)
    (recur)))

(defn start-server
  [port ident action-fn peek-ref]
  (let [ctx (ZMQ/context 1)
        addr (str "tcp://*:" port)]
    (println "starting server on TCP port " port)
    (try
      (with-open [socket (.socket ctx ZMQ/REP)]
        (doto socket
          (.bind addr)
          (.setSendTimeOut 2000))
        (binding [*out* *err*]
          (run-server socket ident action-fn peek-ref)))
      (finally
        (println "closing ZMQ context")
        (.term ctx)))))
