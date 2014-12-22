(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.tools.trace]))

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

(defn ^:dynamic sufficient-neighbors [doublet]
  "Neighbors that is closer to word2"
  (let [head  (take ((comp dec count) doublet) doublet)
        word1 (last head)
        word2 (last doublet)]
   (filter
    (fn [candidate]
      (and
       (>= (distance word1 word2) (distance word2 candidate))
       (= 1 (distance word1 candidate))
       (not (some #{candidate} head))))
      (words-of-size (count word1)))))

(defn ^:dynamic doublets [& doublet]
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

(clojure.tools.trace/dotrace [doublets] (doublets "head" "tall"))
(clojure.tools.trace/dotrace [doublets sufficient-neighbors] (doublets "door" "lock"))


