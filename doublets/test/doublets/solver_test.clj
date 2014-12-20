(ns doublets.solver-test
  (:require [clojure.test :refer :all]
            [doublets.solver :refer :all]))

(deftest solver-test
  (testing "distance"
    (is (= 1 (distance "heal" "teal")))
    (is (= 1 (distance "head" "heal"))))

  (testing "with word links found"
    (is (= ["head" "heal" "teal" "tell" "tall" "tail"]
           (doublets "head" "tail")))

    #_(is (= ["door" "boor" "book" "look" "lock"]
           (doublets "door" "lock")))

    #_(is (= ["bank" "bonk" "book" "look" "loon" "loan"]
           (doublets "bank" "loan")))

    #_(is (= ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
           (doublets "wheat" "bread"))))

  #_(testing "with no word links found"
    (is (= []
           (doublets "ye" "freezer"))))
  )
