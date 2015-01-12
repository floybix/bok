(ns org.nfrac.hatto.cljplayer
  (:require [zeromq.zmq :as zmq]
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

(defn run-server
  "Receives messages on the socket in an infinite loop. The atom
   `peek-ref` stores state for any active bouts, in a map keyed by
   bout id."
  [socket ident action-fn peek-ref]
  (let [bouts peek-ref]
    (while true
      (let [msg (recv-msg socket)]
        (println msg)
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
                      (swap! bouts dissoc bout-id)))))))

(defn start-server
  [port ident action-fn peek-ref]
  (let [ctx (zmq/context 1)
        port (or port (zmq/first-free-port))
        addr (str "tcp://*:" port)]
    (println "starting server on TCP port " port)
    (with-open [socket (doto (zmq/socket ctx :rep)
                         (zmq/bind addr))]
      (binding [*out* *err*]
        (run-server socket ident action-fn peek-ref)))))
