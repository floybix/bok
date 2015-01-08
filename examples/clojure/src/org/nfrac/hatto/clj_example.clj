(ns org.nfrac.hatto.clj-example
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

(defn my-action-fn
  [{:keys [current] :as state}]
  (assoc state
    :actions {:limb-a-rj -5
              :limb-b-rj 0}))

(defn -main
  [& [port more-args]]
  (let [ctx (zmq/context 1)
        port (or port (zmq/first-free-port))
        addr (str "tcp://*:" port)]
    (println "starting server on TCP port " port)
    (with-open [socket (doto (zmq/socket ctx :rep)
                         (zmq/bind addr))]
      (loop [bouts {}]
        (let [msg (recv-msg socket)]
          (println msg)
          (case (:type msg)
            :identify (do
                        (send-msg socket {:type :ident
                                          :player-type :nin
                                          :name "Hatto clj-example"
                                          :author "Felix Andrews <felix@nfrac.org>"
                                          :version "0.1.0-SNAPSHOT"
                                          :hatto-version "0.1.0-SNAPSHOT"})
                        (recur bouts))
            :invite (let [bout-id (:bout-id msg)]
                      (send-msg socket {:type :join
                                        :bout-id bout-id})
                      (recur (assoc bouts bout-id {})))
            :react (let [bout-id (:bout-id msg)
                         last-state (bouts bout-id)
                         state (my-action-fn (assoc last-state
                                               :current (:data msg)))]
                     (send-msg socket {:type :actions
                                       :data (:actions state)})
                     (recur (assoc bouts bout-id state)))
            :finished (let [bout-id (:bout-id msg)]
                        (send-msg socket {:type :bye})
                        (recur (dissoc bouts bout-id)))))))))
