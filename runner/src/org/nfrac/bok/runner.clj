(ns org.nfrac.bok.runner
  (:require [org.nfrac.bok.games :as games]
            [org.nfrac.bok.game-arenas] ;; load games
            [org.nfrac.bok.tree-diff :as diff]
            [org.nfrac.cljbox2d.core :as core]
            [cognitect.transit :as transit]
            [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]])
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
       (throw (ZMQException. 0))))) ;; TODO

(defn send-msg
  [^ZMQ$Socket socket msg]
  (or (.send socket (to-transit msg))
      (throw (ZMQException. 0)))) ;; TODO

(defn remap
  [f m]
  (zipmap (keys m) (map f (vals m))))

(defn take-actions
  "Calls action functions -- looked up by player key in `action-fns`
  -- with perception data and acts on their response. This is the full
  perception data, not diffs as in the remote case. For local testing
  only."
  [game action-fns]
  (let [perceive (:perceive game)
        act (:act game)
        dead? (:dead-players game)]
    (reduce (fn [game [player-key action-fn]]
              (if (dead? player-key)
                (act game player-key {})
                (let [actions (action-fn (perceive game player-key))]
                  (act game player-key actions))))
            game
            action-fns)))

(defn take-remote-actions
  "Sends off diffs of each player's perception data and acts on their
   response."
  [game]
  (let [{:keys [perceive act player-keys bout-id sockets]} game
        dead? (:dead-players game)
        prev-obs (:current-perception game)
        obs (into {} (for [player-key player-keys]
                       [player-key (perceive game player-key)]))]
    (doseq [[player-key sock] sockets]
      (if (dead? player-key)
        () ;; send msg?
        (let [ob-diff (diff/diff (get prev-obs player-key)
                                 (get obs player-key))]
          (send-msg sock {:type :react
                          :bout-id bout-id
                          :data ob-diff}))))
    (-> (reduce (fn [game [player-key sock]]
                  (if (dead? player-key)
                    (act game player-key {})
                    (let [msg (recv-msg sock)
                          actions (:data msg)]
                      (act game player-key actions))))
                game
                sockets)
        (assoc :current-perception obs))))

(defn snapshot-scene
  [game prev-scene]
  (let [identify (comp (juxt :org.nfrac.bok/entity
                             :org.nfrac.bok/component)
                       core/user-data)]
    (-> (core/snapshot-scene (:world game) prev-scene true identify)
        (dissoc :joints)
        (merge (select-keys game [:game-id
                                  :game-version
                                  :game-type
                                  :dt-secs
                                  :idents
                                  :player-keys
                                  :dead-players
                                  :final-result
                                  :player-raycast
                                  :player-energy
                                  :player-gun])))))

(defn record-scene
  [game]
  (let [prev-scene (:current-scene game {})
        new-scene (snapshot-scene game prev-scene)
        scened (diff/diff prev-scene new-scene)]
    (-> (update-in game [:scene-deltas] #(conj (or % []) scened))
        (assoc :current-scene new-scene))))

(defn step-remote
  [game]
  (let [game-step (:game-step game)]
    (try
      (-> game
          (game-step)
          (take-remote-actions)
          (record-scene))
      (catch ZMQException e
        (assoc game :error e))
      (catch IllegalStateException e
        (assoc game :error e)))))

(defn step-local
  [game action-fns]
  (let [game-step (:game-step game)]
    (-> game
        (game-step)
        (take-actions action-fns)
        (record-scene))))

(defn final-result
  [game]
  (if-let [err (:error game)]
    {:error err, :end-time (:time game)}
    (let [check-end (:check-end game)]
      (when-let [result (check-end game)]
        (assoc result :end-time (:time game))))))

(defn run-bout
  [game]
  (loop [game game]
    (if-let [res (final-result game)]
      (assoc game :final-result res)
      (recur (step-remote game)))))

(defn saved-game-data
  [scene-deltas]
  (let [game-meta {:timestamp (java.util.Date.)
                   :bok-version [0 0 1]}]
    (list game-meta scene-deltas)))

(defn end-bout
  [game]
  (let [{:keys [bout-id sockets]} game
        result (:final-result game)]
    (if (:error result)
      (do
        ;; something
        )
      (let [winner (:winner result)
            msg {:type :final-result, :bout-id bout-id
                 :data result}]
        (when-let [file (:save-out game)]
          (let [data (saved-game-data (:scene-deltas game))]
            (io/copy (to-transit data) (io/file file))))
        (doseq [[player-key sock] sockets
                :let [my-msg (assoc-in msg [:data :my-result]
                                       (case winner
                                         nil :draw
                                         player-key :win
                                         :loss))]]
          (send-msg sock my-msg)
          (assert (= :bye (:type (recv-msg sock)))))))
    game))

(defn start-bout
  [game-id sockmap opts]
  (let [bout-id (str (java.util.UUID/randomUUID))]
    (when-not (:quiet? opts)
      (println "inviting to bout" bout-id))
    (doseq [[k sock] sockmap]
      (send-msg sock {:type :invite, :bout-id bout-id}))
    (doseq [[k ^ZMQ$Socket sock] sockmap]
      (assert (= :join (:type (recv-msg sock))))
      (.setSendTimeOut sock 1000)
      (.setReceiveTimeOut sock 1000))
    (when-not (:quiet? opts)
      (println "identifying players."))
    (doseq [[k sock] sockmap]
      (send-msg sock {:type :identify}))
    (let [idents (remap (fn [sock]
                          (:data (recv-msg sock)))
                        sockmap)
          creature-types (remap :creature-type idents)]
      (when-not (:quiet? opts)
        (println idents))
      (-> (games/build game-id creature-types opts)
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
  ([game-id addrs opts]
   (main (ZMQ/context 1) game-id addrs opts))
  ([^ZMQ$Context ctx game-id addrs opts]
   (with-all-connected ctx ZMQ/REQ addrs
     (fn [socks]
       (let [b (->
                (start-bout game-id (zipmap PLAYER_KEYS socks) opts)
                (run-bout)
                (end-bout))
             res (:final-result b)]
         (when-not (:quiet? opts)
           (println res))
         (when (and (:repeat opts) (not (:error res)))
           (recur socks)))))))
