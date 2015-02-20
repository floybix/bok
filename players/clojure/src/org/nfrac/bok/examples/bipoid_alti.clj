(ns org.nfrac.bok.examples.bipoid-alti
  (:require [org.nfrac.bok.cljplayer :as serv]
            [org.nfrac.bok.cljplayer.util :as util
             :refer [x-val y-val abs turn-towards PI angle-left?]]))

(def ident {:creature-type :bipoid
            :name "Example altitude-playing bipoid"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

;; max torque
(def MT 200.0)

(def down-left (* PI -0.8))
(def down-right (* PI -0.2))

(defn update-upward-check
  "Helps to work out which direction to go in to get higher up.. Map
   `m` should be this function's result from the previous time step,
   and `rc` the raycast perception data. Returns keys `:dir` - which
   direction seems to be upwards: -1 left, 1 right, 0 not yet
   determined; and `:next-rc` - the angle for the next raycast." [m
   rc]
  (if-not rc
    ;; initialisation
    (assoc m
      :next-rc down-left
      :dir 0)
    (let [a (:angle rc)
          left (if (angle-left? a)
                 (or (:distance rc) 100)
                 (:left m))
          right (if (not (angle-left? a))
                  (or (:distance rc) 100)
                  (:right m))]
      (assoc m
        :left left
        :right right
        ;; alternate raycast side
        :next-rc (if (angle-left? a)
                   down-right
                   down-left)
        ;; if left distance is lower (i.e. left is upwards), go there.
        :dir (if (and left right)
               (if (< left right) -1 1)
               ;; return dir 0 if raycasts not completed yet
               0)))))

(defn my-action-fn
  [state]
  (let [{:keys [entities my-key other-players]} (:current state)
        opp-key (first other-players)
        me (get entities my-key)
        {:keys [head leg-a1 leg-a2 leg-b1 leg-b2]} (:components me)
        eye (first (:points head))
        ;; look up and compare raycast distances to decide direction
        upward-check (-> (:upward-check state {})
                         (update-upward-check (:raycast (:current state))))
        dir (:dir upward-check)
        BRACE -0.7
        actions
        {:joint-motors
         {:leg-b1 [(* -5 dir) MT]
          :leg-a2 (turn-towards (* dir BRACE) (:angle leg-a2) 0 10)
          :leg-b2 (turn-towards (* dir BRACE) (:angle leg-b2) 0 10)
          }
         :raycast (:next-rc upward-check)}]
    (assoc state
      :actions actions
      :upward-check upward-check)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
