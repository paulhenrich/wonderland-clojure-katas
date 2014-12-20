(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn doublets [word1 word2]
  "make me work")

(defn distance [word1 word2]
  (count
   (filter
    identity
    (for [letter1 word1
          letter2 word2]
      (= letter1 letter2)))))

(defn neighbors [word1 word2]
  (filter
   (fn [candidate]
     (= 1 (distance word1 candidate)))
   words))

(neighbors "heal" "feel")
