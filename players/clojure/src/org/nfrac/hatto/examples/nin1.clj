(ns org.nfrac.hatto.examples.nin1
  (:require [org.nfrac.hatto.cljplayer :as serv]
            [zeromq.zmq :as zmq]))

(def ident {:creature-type :nin
            :name "Example nin1"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :hatto-version "0.1.0-SNAPSHOT"})

(defn my-action-fn
  [{:keys [current] :as state}]
  (let [{:keys [my-key entities]} current
        me (get entities my-key)
        other-keys (keys (dissoc entities my-key :arena))
        my-head (:head (:components me))
        eye (:position my-head)
        eye-vel (:velocity my-head)]
   (assoc state
     :actions {:limb-a-rj -5
               :limb-b-rj 0})))

(defn -main
  [& [port more-args]]
  (let [ctx (zmq/context 1)
        port (or port (zmq/first-free-port))
        addr (str "tcp://*:" port)]
    (println "starting server on TCP port " port)
    (with-open [socket (doto (zmq/socket ctx :rep)
                         (zmq/bind addr))]
      (serv/run-server socket ident my-action-fn)
      )))
