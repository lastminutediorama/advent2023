(ns advent2023.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str :refer [split-lines]])
  (:gen-class))

(defn extract-nums [s]
  (let [digs (filter #(Character/isDigit %) (vec s))]
    (Integer/parseInt (str (first digs) (last digs)))))

(defn day1-1 []
  (let [data-file (slurp (io/resource "day01.txt"))
        lines (split-lines data-file)]
    (apply + (map extract-nums
                  lines))))

(def strnums {"one" 1
              "two" 2
              "three" 3
              "four" 4
              "five" 5
              "six" 6
              "seven" 7
              "eight" 8
              "nine" 9
              "ten" 10})

(defn find-all-numstrs [haystack needle]
  (into {} (reduce (fn [acc i]
                     (if (= (subs haystack i (+ i (count needle))) needle)
                       (conj acc {i (get strnums needle)})
                       acc))
                   []
                   (range (- (count haystack) (dec (count needle)))))))

(defn find-strs [s]
  (into {} (remove
            nil?
            (mapv #(find-all-numstrs s %) (keys strnums)))))

(defn find-nums [s]
  (into {} (remove nil? 
                   (for [i (range (count s))]
                          (if (Character/isDigit (nth (vec s) i))
                            {i (Integer/parseInt (str (nth (vec s) i)))})))))

; Here we combine the maps of all found nums and their locs
(defn full-map [s]
  (sort (merge (find-nums s)
               (find-strs s))))

(defn day1-2 []
  (let [data-file (slurp (io/resource "day01.txt"))
        lines (split-lines data-file)]
    (apply + (map
              (fn [ln]
                (let [numlocs (full-map ln)]
                 (let [res (Integer/parseInt (str (val (first numlocs))
                                                   (val (last numlocs))))]
                    res))) lines))))

(comment
  (day1-1)
  (day1-2)
  #_end)
