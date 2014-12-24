(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))


(defn words-of-size [size]
  (filter (fn [word] (= (count word) size)) words))

(defn distance [word1 word2]
  (count (filter
          (fn [letter-pair]
            (not= (first letter-pair)
                  (second letter-pair)))
          (partition 2 (interleave word1 word2)))))

(defn distance-from [target]
  "Makes comparators that sort by distance from target"
  (comparator (fn [word1 word2]
                 (< (distance target word1) (distance target word2)))))

(defn sufficient-neighbors [doublet]
  "Neighbors ordered by closeness to word2"
  (let [head  (take ((comp dec count) doublet) doublet)
        word1 (last head)
        word2 (last doublet)]
   (sort (distance-from word2) (filter
    (fn [candidate]
      (and
       (= 1 (distance word1 candidate))
       (not (some #{candidate} head))))
      (words-of-size (count word1))))))

(defn doublets [& doublet]
  (let [head  (take ((comp dec count) doublet) doublet)
        word1 (last head)
        word2 (last doublet)]
  (cond
   (= (distance word1 word2) 1)
   doublet
   (= 0 (count (sufficient-neighbors doublet)))
   '()
   :else
    (apply doublets (flatten (cons head
              (list (first (sufficient-neighbors doublet)) word2)))))))

#_(

  (doublets "door" "lock")
  (doublets "wheat" "bread")
  (doublets "bank" "loan")
  (doublets "door" "lock")
)
