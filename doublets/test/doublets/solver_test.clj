(ns doublets.solver-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [doublets.solver :refer :all]))

(deftest solver-test
  (with-redefs [doublets.solver/de-rigueur-words-used-in-good-society
                (-> "words.edn"
                    (io/resource)
                    (slurp)
                    (read-string))]
    (testing "distance"
      (is (= 1 (distance "heal" "teal")))
      (is (= 1 (distance "head" "heal"))))

    (testing "with word links found"
      (is (= ["head" "heal" "teal" "tell" "tall" "tail"]
             (doublets "head" "tail")))

      (is (= ["door" "boor" "book" "look" "lock"]
             (doublets "door" "lock")))

      (is (= ["bank" "bonk" "book" "look" "loon" "loan"]
             (doublets "bank" "loan")))

      (is (= ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
             (doublets "wheat" "bread"))))

    (testing "with no word links found"
      (is (= []
             (doublets "ye" "freezer"))))
    ))
