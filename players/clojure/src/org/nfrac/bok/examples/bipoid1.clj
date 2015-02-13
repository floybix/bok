(ns org.nfrac.bok.examples.bipoid1
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [x-val y-val abs turn-towards]]))

(def ident {:creature-type :bipoid
            :name "Example bipoid1"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

;; max torque
(def MT 100.0)

(defn my-action-fn-opp
  [state]
  (let [{:keys [entities my-key other-players]} (:current state)
        opp-key (first other-players)
        me (get entities my-key)
        {:keys [head leg-a1 leg-a2 leg-b1 leg-b2]} (:components me)
        eye (first (:points head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        dir (if (< (x-val (:position opp-eye))
                   (x-val (:position eye))) -1 1)
        actions
        {:joint-motors
         {:leg-a1 [(* -5 dir) MT]
          :leg-b1 [(* -4 dir) MT]
          :leg-a2 (turn-towards 0 (:angle leg-a2) 0 30)
          :leg-b2 (turn-towards 0 (:angle leg-b2) 0 30)
          }}]
    (assoc state
      :actions actions)))

(defn my-action-fn
  [state]
  (let [{:keys [entities other-players]} (:current state)
        opp-key (first other-players)]
    (if-not (get entities opp-key)
      ;; can not see opponent. do nothing
      state
      (my-action-fn-opp state))))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
