(ns org.nfrac.hatto.cljplayer.util)

;; TODO euclidean

(defn infer-derived-measures
  [{pos :position, vel :velocity} eye eye-vel]
  (let [rel-pos (v-sub pos eye)
        rel-vel (v-sub vel eye-vel)]
    (assoc info
      :relative-position rel-pos
      :angle-from-me (v-angle rel-pos)
      :distance (v-dist pos eye)
      :relative-velocity rel-vel
      :velocity-angle-to-me (v-angle (v-scale (v-sub rel-vel rel-pos) -1)))))
