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
  [socket ident action-fn]
  (loop [bouts {}]
    (let [msg (recv-msg socket)]
      (println msg)
      (case (:type msg)
        :identify (do
                    (send-msg socket (assoc ident
                                       :type :ident))
                    (recur bouts))
        :invite (let [bout-id (:bout-id msg)]
                  (send-msg socket {:type :join
                                    :bout-id bout-id})
                  (recur (assoc bouts bout-id {})))
        :react (let [bout-id (:bout-id msg)
                     last-state (bouts bout-id)
                     state (action-fn (assoc last-state
                                        :current (:data msg)))]
                 (send-msg socket {:type :actions
                                   :data (:actions state)})
                 (recur (assoc bouts bout-id state)))
        :finished (let [bout-id (:bout-id msg)]
                    (send-msg socket {:type :bye})
                    (recur (dissoc bouts bout-id)))))))

