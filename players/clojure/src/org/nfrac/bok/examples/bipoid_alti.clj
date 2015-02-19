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

(defn update-grid-cache
  [grid-cache grid-pos rc]
  (if-not rc
    grid-cache
    (let [this-cache (get grid-cache grid-pos {})
          a (:angle rc)
          left (if (angle-left? a)
                 (or (:distance rc) 100)
                 (:left this-cache))
          right (if (not (angle-left? a))
                  (or (:distance rc) 100)
                  (:right this-cache))]
      (assoc grid-cache grid-pos
             {:left left :right right
              ;; if left distance is less (i.e. upwards), go there
              :dir (if (and left right)
                     (if (< left right) -1 1)
                     ;; return dir 0 if raycasts not completed yet
                     0)}))))

(defn my-action-fn
  [state]
  (let [{:keys [entities my-key other-players]} (:current state)
        opp-key (first other-players)
        me (get entities my-key)
        {:keys [head leg-a1 leg-a2 leg-b1 leg-b2]} (:components me)
        eye (first (:points head))
        ;; treat positions as comparable if within the integer meter
        grid-pos (mapv int (:position eye))
        ;; look up and compare distance at down-left vs down-right
        grid-cache (-> (:grid-cache state {})
                       (update-grid-cache grid-pos (:raycast (:current state))))
        dir (get-in grid-cache [grid-pos :dir] 0)
        last-rc-side (:last-rc-side state :left)
        this-rc-side (case last-rc-side :left :right, :right :left)
        BRACE -0.7
        actions
        {:joint-motors
         {:leg-b1 [(* -5 dir) MT]
          :leg-a2 (turn-towards (* dir BRACE) (:angle leg-a2) 0 10)
          :leg-b2 (turn-towards (* dir BRACE) (:angle leg-b2) 0 10)
          }
         :raycast (case this-rc-side
                    :left down-left
                    :right down-right)}]
    (assoc state
      :actions actions
      :last-rc-side this-rc-side
      :grid-cache grid-cache)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
