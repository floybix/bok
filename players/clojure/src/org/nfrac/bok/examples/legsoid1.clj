(ns org.nfrac.bok.examples.legsoid1
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [abs angle-left? turn-towards]]))

(def ident {:creature-type :legsoid
            :name "Example legsoid1"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

;; max torque
(def MT 100.0)

(defn my-action-fn
  [state]
  (let [{:keys [my-key entities]} (:current state)
        [opp-key] (keys (dissoc entities my-key :arena))
        me (get entities my-key)
        {:keys [head leg-a1 leg-a2 leg-b1 leg-b2]} (:components me)
        eye (first (:points head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        opp-ft (util/point-features opp-eye eye)
        ground (get-in entities [:arena :components :ground])
        edge-fts (map #(util/point-features % eye)
                      (:points ground))
        opp-angle (:angle-from-me opp-ft)
        dir (if (angle-left? opp-angle) -1 1)
        actions
        {:joint-motors
         {:leg-a1 [(* -5 dir) MT]
          :leg-b1 [(* -4 dir) MT]
          :leg-a2 (turn-towards 0 (:angle leg-a2) 0 30)
          :leg-b2 (turn-towards 0 (:angle leg-b2) 0 30)
          }}]
    (assoc state
      :actions actions)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
