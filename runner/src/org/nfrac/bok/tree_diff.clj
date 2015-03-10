(ns org.nfrac.bok.tree-diff)

(defn diff
  "Compares twos trees -- nested maps -- and returns another tree
   representing the differences between them, such that it can be
   recursively merged into the old one to produce the new one. Removed
   values are represented by nils, so this assumes that the existence
   of a nil value is equivalent to the corresponding key not existing.

   Similar to `differ/diff` but does not diff sequences or sets, and
   does not explicitly represent removals. Motivation: this is for
   transit over the wire to other languages, so should be as easy as
   possible to implement the patching step in other languages. Also
   the Bok use case involves lots of position vectors [x y] which
   always change together; sequence diffs are counter productive."
  [state new-state]
  (loop [more-ks (distinct (concat (keys state)
                                   (keys new-state)))
         m (transient {})]
    (if (empty? more-ks)
      (persistent! m)
      (let [[k & ks] more-ks
            old-val (get state k)
            new-val (get new-state k)]
        (cond (= old-val new-val)
              (recur ks m)
              (and (map? old-val)
                   (map? new-val))
              (recur ks (assoc! m k (diff old-val new-val)))
              :else
              (recur ks (assoc! m k new-val)))))))

(defn patch
  "Applies a `diff` map to the original map `m`. Like merge, but
   merges maps recursively."
  [m diff]
  (if (and (map? m) (map? diff))
    (merge-with patch m diff)
    diff))
