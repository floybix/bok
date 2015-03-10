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
    :default "game.bok"]]
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
    ;; Handle help and error conditions
    (cond
     (:help options) (exit 0 (usage summary))
     (< (count arguments) 1) (exit 1 (usage summary))
     errors (exit 1 (error-msg errors))
     (not (get (known-game-ids) game-id)) (exit 1 (error-msg
                                                   [(str "Unknown game " game-id)])))
    (let [ctx (ZMQ/context 1)
          options (set/rename-keys options
                                   {:time-limit :game-over-secs})]
      (try
        (println options)
        (if (:no-vis options)
          (runner/main ctx game-id addrs options)
          (vrunner/main ctx game-id addrs options)
          )
        (finally
          (.term ctx))))))
