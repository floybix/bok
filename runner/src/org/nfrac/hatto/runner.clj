(ns org.nfrac.hatto.runner
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

(defn -main
  [addr-a addr-b & more-args]
  (let [ctx (zmq/context 1)
        bout-id (str (java.util.UUID/randomUUID))]
    (println "connecting to" addr-a)
    (println "connecting to" addr-b)
    (with-open [sock-a (doto (zmq/socket ctx :req)
                         (zmq/connect addr-a))
                sock-b (doto (zmq/socket ctx :req)
                         (zmq/connect addr-b))]
      (println "connected.")
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
        ;; TODO create world
        (let []
          )))))
