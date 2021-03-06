(ns org.nfrac.bok.examples.wormoid1
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [x-val y-val abs turn-towards reverse-turn-towards
                     PI HALF_PI]]))

(def ident {:creature-type :wormoid
            :name "Example wormoid1"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

;; max torque
(def MT 50.0)

(defn my-action-fn-opp
  [state]
  (let [{:keys [entities my-key other-players]} (:current state)
        opp-key (first other-players)
        me (get entities my-key)
        ;; merge joint info into corresponding named components:
        {:keys [head seg-1 seg-2 seg-3 seg-4 seg-5]}
        (merge-with merge ;; merge nested maps
                    (:components me)
                    (:joints me))
        eye (first (:points head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        opp-xoff (- (x-val (:position opp-eye))
                    (x-val (:position eye)))
        dir (if (neg? opp-xoff) -1 1)
        actions
        (cond
         ;; opponent right here; spring up
         (< (abs opp-xoff) 1.0)
         {:joint-motors
          {:seg-2 (turn-towards 0 (:joint-angle seg-2) 0 30)
           :seg-3 (turn-towards 0 (:joint-angle seg-3) 0 30)
           :seg-4 (turn-towards 0 (:joint-angle seg-4) 0 30)
           :seg-5 (turn-towards 0 (:joint-angle seg-5) 0 30)
           }}
         ;; lie in wait
         (< (abs opp-xoff) 3.5)
         {:joint-motors
          {:seg-2 (turn-towards PI (:joint-angle seg-2) 0 3)
           :seg-3 (turn-towards PI (:joint-angle seg-3) 0 10)
           :seg-4 (turn-towards PI (:joint-angle seg-4) 0 5)
           :seg-5 (turn-towards PI (:joint-angle seg-5) 0 5)
           }}
         ;; move towards opponent
         :else
         {:joint-motors
          ;; face opponent
          {:seg-2 (reverse-turn-towards (* dir (- HALF_PI)) (:angle seg-1) 0 10)
           :seg-3 [(* -5 dir) MT]
           :seg-4 nil
           :seg-5 (turn-towards (* dir (- HALF_PI)) (:angle seg-5) 0 10)}})]
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
