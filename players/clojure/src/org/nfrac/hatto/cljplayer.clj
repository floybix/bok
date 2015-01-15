(ns org.nfrac.hatto.cljplayer
  (:require [cognitect.transit :as transit])
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
  (or (from-transit (.recv socket))
      (throw (Exception. "ZMQ recv failed."))))

(defn send-msg
  [^ZMQ$Socket socket msg]
  (or (.send socket (to-transit msg))
      (throw (Exception. "ZMQ send failed."))))

(defn run-server
  "Receives messages on the socket in an infinite loop. The atom
   `peek-ref` stores state for any active bouts, in a map keyed by
   bout id."
  [socket ident action-fn peek-ref]
  (let [bouts peek-ref]
    (while true
      (let [msg (recv-msg socket)]
        (case (:type msg)
          :identify (send-msg socket (assoc ident
                                       :type :ident))
          :invite (let [bout-id (:bout-id msg)]
                    (send-msg socket {:type :join
                                      :bout-id bout-id})
                    (swap! bouts assoc bout-id {}))
          :react (let [bout-id (:bout-id msg)
                       last-state (@bouts bout-id)
                       state (action-fn (assoc last-state
                                          :current (:data msg)))
                       actions (:actions state)]
                   (send-msg socket {:type :actions
                                     :data actions})
                   (swap! bouts assoc bout-id state))
          :finished (let [bout-id (:bout-id msg)]
                      (send-msg socket {:type :bye})
                      (swap! bouts dissoc bout-id))
          (println "Unrecognised message type:" msg))))))

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
