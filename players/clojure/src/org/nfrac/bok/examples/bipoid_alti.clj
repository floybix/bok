(ns org.nfrac.bok.examples.bipoid-alti
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [x-val y-val abs turn-towards slope-check upward-check]]))

(def ident {:creature-type :bipoid
            :name "Example altitude-playing bipoid"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

;; max torque
(def MT 200.0)

(defn my-action-fn
  [state]
  (let [{:keys [entities my-key other-players]} (:current state)
        opp-key (first other-players)
        me (get entities my-key)
        {:keys [head leg-a1 leg-a2 leg-b1 leg-b2]} (:components me)
        eye (first (:points head))
        ;; look up and compare raycast distances to decide direction
        upward (upward-check (:raycast (:current state)) (:upward state))
        dir (:dir upward)
        BRACE -0.7
        actions
        {:joint-motors
         {:leg-b1 [(* -5 dir) MT]
          :leg-a2 (turn-towards (* dir BRACE) (:angle leg-a2) 0 10)
          :leg-b2 (turn-towards (* dir BRACE) (:angle leg-b2) 0 10)
          }}]
    (assoc state
      :actions (assoc actions :raycast (:next-rc upward))
      :upward upward)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
