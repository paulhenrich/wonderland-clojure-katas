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


(defn neighbors [word1]
  "A neighbor is a word with a distance of one"
  (filter
   (fn [candidate]
     (= 1 (distance word1 candidate)))
   (words-of-size (count word1))))

(defn ^:dynamic sufficient-neighbor [word1 word2]
  "Neighbor that is closer to word2"
  (first (filter
   (fn [candidate]
     (> (distance word1 word2) (distance word2 candidate)))
   (neighbors word1))))

(defn ^:dynamic doublets-seq [& doublet]
  (let [head  (take ((comp dec count) doublet) doublet)
        word1 (last head)
        word2 (last doublet)]
    (cond
     (some #{word2} (neighbors word1))
     (conj head word2)
     (= nil (sufficient-neighbor word1 word2))
     nil
     :else
     (apply doublets-seq (conj head (sufficient-neighbor word1 word2))))))

(clojure.tools.trace/dotrace [doublets-seq
                              sufficient-neighbor] (doublets-seq "head" "tail"))
(defn doublets [& doublet]
  (let [head  (take ((comp dec count) doublet) doublet)
        word1 (last head)
        word2 (last doublet)]
  (cond
   (= (distance word1 word2) 1)
   doublet
   (= 0 (count (sufficient-neighbor word1 word2)))
   '()
   :else
    (apply doublets (flatten (cons head
              (list (sufficient-neighbor word1 word2) word2)))))))

(doublets "head" "tall")
(doublets "tall" "tail")
(sufficient-neighbor "tell" "tail")
(doublets "door" "lock")
(some #{"foo"} ["foo" "baz"])
(sufficient-neighbor "aaaaaa" "bbbbbbb")
(take ((comp dec count) [:a :b :c]) [:a :b :c])

(doublets "head" "heal")
(def test-words ["head" "teal"])
(cons (take 1 test-words)
      (cons (sufficient-neighbor (first test-words) (second test-words)) (rest test-words)))
