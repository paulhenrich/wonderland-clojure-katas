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


(defn neighbors [word1]
  "A neighbor is a word with a distance of one"
  (filter
   (fn [candidate]
     (= 1 (distance word1 candidate)))
   (words-of-size (count word1))))

(defn candidate-neighbors [word1 word2]
  "Neighbors that share a letter with the last word"
  (filter
   (fn [candidate]
     (< (distance word1 candidate) (distance word2 candidate)))
   (neighbors word1)))

(candidate-neighbors "head" "tell")

(neighbors "heal")

(neighbors "head")

(defn doublets [word1 word2]
  "make me work")
