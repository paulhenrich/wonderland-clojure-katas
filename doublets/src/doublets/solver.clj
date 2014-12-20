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
  "A neighbor is a word with an edit distance of one"
  (filter
   (fn [candidate]
     (= 1 (distance word1 candidate)))
   (words-of-size (count word1))))


(neighbors "heal")

(neighbors "head")

(defn doublets [word1 word2]
  "make me work")
