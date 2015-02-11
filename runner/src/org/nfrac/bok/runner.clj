(ns org.nfrac.bok.runner
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.cljbox2d.core :as core]
            [cognitect.transit :as transit]
            [differ.core :as differ])
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

(defn remap
  [f m]
  (zipmap (keys m) (map f (vals m))))

(defn take-actions
  "Calls action functions (with perception data) looked up by player
   key in `action-fns`. For local testing only."
  [game action-fns]
  (if (games/act-now? game)
    (let [perceive (:perceive game)
          act (:act game)
          dead? (:dead-players game)]
      (-> (reduce (fn [game [player-key action-fn]]
                    (if (dead? player-key)
                      (act game player-key {})
                      (let [actions (action-fn (perceive game player-key))]
                        (act game player-key actions))))
                  game
                  action-fns)
          (assoc :last-act-time (:time game))))
    game))

(defn take-remote-actions
  [game]
  (if (games/act-now? game)
    (let [{:keys [perceive act player-keys bout-id sockets]} game
          dead? (:dead-players game)]
      (doseq [[player-key sock] sockets]
        (if (dead? player-key)
          () ;; send msg?
          (let [ob (perceive game player-key)]
            (send-msg sock {:type :react
                            :bout-id bout-id
                            :data ob}))))
      (-> (reduce (fn [game [player-key sock]]
                    (if (dead? player-key)
                      (act game player-key {})
                      (let [msg (recv-msg sock)
                            actions (:data msg)]
                        (act game player-key actions))))
                  game
                  sockets)
          (assoc :last-act-time (:time game))))
    game))

(defn snapshot-scene
  [game prev-scene]
  (let [identify (comp (juxt :org.nfrac.bok.games/entity
                             :org.nfrac.bok.games/component)
                       core/user-data)]
    (-> (core/snapshot-scene (:world game) prev-scene true identify)
        (merge (select-keys game [:player-keys
                                  :dead-players
                                  :final-result
                                  :time
                                  :player-energy
                                  :player-gun])))))

(defn record-scene
  [game]
  (let [prev-scene (:current-scene game {})
        new-scene (snapshot-scene game prev-scene)
        scened (differ/diff prev-scene new-scene)]
    (-> (update-in game [:scene-deltas] conj scened)
        (assoc :current-scene new-scene))))

(defn step-remote
  [game]
  (let [world-step (:world-step game)]
    (try
      (-> game
          (world-step)
          (take-remote-actions)
          (record-scene))
      (catch ZMQException e
        (assoc game :error e)))))

(defn step-local
  [game action-fns]
  (let [world-step (:world-step game)]
    (-> game
        (world-step)
        (take-actions action-fns)
        (record-scene))))

(defn final-result
  [game]
  (if-let [err (:error game)]
    {:error err}
    (let [check-end (:check-end game)]
      (when-let [result (check-end game)]
        (assoc result :end-time (:time game))))))

(defn run-bout
  [game]
  (loop [game game]
    (if-let [res (final-result game)]
      (assoc game :final-result res)
      (recur (step-remote game)))))

(defn end-bout
  [game]
  (let [{:keys [bout-id sockets]} game
        result (:final-result game)]
    (if (:error result)
      (do
        ;; something
        )
      (let [winner (:winner result)
            msg {:type :finished, :bout-id bout-id
                 :winner winner}]
        (doseq [[player-key sock] sockets]
          (send-msg sock msg)
          (assert (= :bye (:type (recv-msg sock)))))))
    game))

(defn start-bout
  [arena-type sockmap opts]
  (let [bout-id (str (java.util.UUID/randomUUID))]
    (println "inviting to bout" bout-id)
    (doseq [[k sock] sockmap]
      (send-msg sock {:type :invite, :bout-id bout-id}))
    (doseq [[k ^ZMQ$Socket sock] sockmap]
      (assert (= :join (:type (recv-msg sock))))
      (.setSendTimeOut sock 1000)
      (.setReceiveTimeOut sock 1000))
    (println "identifying players.")
    (doseq [[k sock] sockmap]
      (send-msg sock {:type :identify}))
    (let [idents (remap (fn [sock]
                          (recv-msg sock))
                        sockmap)
          creature-types (remap :creature-type idents)]
      (println idents)
      (-> (games/build arena-type creature-types opts)
          (assoc :bout-id bout-id
                 :idents idents
                 :sockets sockmap)))))

(defn with-all-connected
  "Creates a ZMQ socket of type `type` connected to each address in
   `addrs`, and passes the sequence of open sockets to `f`. Closes all
   sockets in a finally block."
  [^ZMQ$Context ctx type addrs f]
  (let [open-socks (atom [])]
    (try
      (doseq [addr addrs]
        (let [sock (.socket ctx type)]
          (swap! open-socks conj sock)
          (.connect sock addr)))
      (f @open-socks)
      (finally
        (doseq [sock @open-socks]
          (.close ^ZMQ$Socket sock))))))

(def PLAYER_KEYS
  (map #(keyword (str "player-" %)) [\a \b \c \d \e \f \g \h \i \j]))

(defn main
  [^ZMQ$Context ctx arena-type addrs opts]
  (with-all-connected ctx ZMQ/REQ addrs
    (fn [socks]
      (-> (start-bout arena-type (zipmap PLAYER_KEYS socks) opts)
          (run-bout)
          (end-bout)))))

(defn -main
  [arena-type & addrs]
  (let [ctx (ZMQ/context 1)
        arena-type (keyword arena-type)]
    (assert (pos? (count addrs)))
    (println "connecting to" addrs)
    (try
      (-> (main ctx arena-type addrs {})
          :final-result
          (println))
      (finally
        (println "closing ZMQ context")
        (.term ctx)))))
