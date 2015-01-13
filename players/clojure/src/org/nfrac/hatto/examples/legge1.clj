(ns org.nfrac.hatto.examples.legge1
  (:require [org.nfrac.hatto.cljplayer :as serv]
            [org.nfrac.hatto.cljplayer.util :as util
             :refer [abs angle-left? angle-up? turn-down]]))

(def ident {:creature-type :legge
            :name "Example legge1"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :hatto-version "0.1.0-SNAPSHOT"})

(defn my-action-fn
  [state]
  (let [{:keys [my-key entities]} (:current state)
        [opp-key] (keys (dissoc entities my-key :arena))
        me (get entities my-key)
        my-head (:head (:components me))
        eye (first (:points my-head))
        opp (get entities opp-key)
        opp-head (:head (:components opp))
        opp-eye (first (:points opp-head))
        opp-ft (util/point-features opp-eye eye)
        ground (get-in entities [:arena :components :ground])
        edge-fts (map #(util/point-features % eye)
                      (:points ground))
        opp-angle (:angle-from-me opp-ft)
        dir (if (angle-left? opp-angle) 1 -1)
        a2-angle (-> me :components :limb-a2 :angle)
        b2-angle (-> me :components :limb-b2 :angle)
        a2-sp (turn-down a2-angle 0.25)
        b2-sp (turn-down b2-angle 0.25)
        actions {:limb-a1-rj 1
                 :limb-b1-rj 2
                 :limb-a2-rj a2-sp
                 :limb-b2-rj b2-sp}]
    (assoc state
      :actions actions)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (serv/start-server port ident #'my-action-fn peek-ref))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
