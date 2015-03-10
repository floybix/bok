(ns org.nfrac.bok.main
  (:require [org.nfrac.bok.runner :as runner]
            [org.nfrac.bok.visual-runner :as vrunner]
            [org.nfrac.bok.games :as games :refer [known-game-ids]]
            [org.nfrac.bok.game-arenas] ;; load games
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [clojure.set :as set])
  (:import [org.zeromq ZMQ]))

(def cli-options
  [["-n" "--no-vis" "Run in shell only, no visual display window."]
   ["-f" "--fast-vis" "Start visual display in fast mode."]
   ["-r" "--repeat" "Start a new game when one ends."]
   ["-t" "--time-limit SECS" "Game over time in seconds, 0 for unlimited."
    :default 60
    :parse-fn #(let [i (Integer/parseInt %)] (when (pos? i) i))]
   ["-o" "--save-out FILE" "File to save the game recording to."
    :default "game.bok"]
   [nil "--replay FILE" "Just replay a recorded game from file."]]
  )

(defn usage [options-summary]
  (->> ["Bok runner."
        "Connects to players, sends them perception, applies actions, steps world."
        ""
        "Usage: bok [options] game addresses..."
        ""
        "Available games:"
        (->> (known-game-ids)
             (sort)
             (map name)
             (string/join \newline))
        ""
        "Addresses are like tcp://host:port"
        "Bok players must bind ZMQ sockets at the given addresses."
        ""
        "Options:"
        options-summary
        ""]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        [game-id & addrs] arguments
        game-id (keyword game-id)]
    ;; handle help and error conditions
    (cond
     (:help options) (exit 0 (usage summary))
     (and (not game-id) (not (:replay options))) (exit 1 (usage summary))
     errors (exit 1 (error-msg errors)))
    ;; replay is different
    (if (:replay options)
      (vrunner/replay-game-from-file (:replay options))
      ;; actual run
      (let [options (set/rename-keys options
                                     {:time-limit :game-over-secs})]
        (when (not (get (known-game-ids) game-id))
          (exit 1 (error-msg [(str "Unknown game " game-id)])))
        (let [ctx (ZMQ/context 1)]
          (try
            (if (:no-vis options)
              (runner/main ctx game-id addrs options)
              (vrunner/main ctx game-id addrs options))
            (finally
              (.term ctx))))))))
