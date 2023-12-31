(ns advent2023.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str :refer [split-lines]]))

(def found-gears (atom []))

(defn coords->digits [coords arr2d]
  (Integer/parseInt
   (clojure.string/join ""
                        (mapv (fn [[r c]]
                                (aget arr2d r c)) coords))))

(defn near-symbol? [r-above c-left r-below c-right arr2d & n]
  (let [min-row (max r-above 0)
        max-row (min r-below (dec (alength arr2d)))
        min-col (max c-left 0)
        max-col (min c-right (dec (alength arr2d)))
        coords (for [row (range min-row (inc max-row))
                     col (range min-col (inc max-col))]
                 [row col])]
    ; cycle through coords, and if any are a symbol, we collect it
    (let [symbols (filter (fn [[r c]]
                            (let [x (aget arr2d r c)]
                              (when (= x \*) ;; For Part 2
                                (swap! found-gears conj {:gearloc [r c]
                                                         :num (coords->digits (first n) arr2d)}))
                              (not (or (Character/isDigit x) (= x \.)))))
                          coords)]
      (seq symbols))))


(defn find-parts
  "checks vec of coords (ncoll) for neighboring symbols"
  [ncoll arr2d]
  (reduce (fn [parts n]
            (let [rowabove (dec (first (first n)))
                  rowbelow (inc (first (first n)))
                  colleft (dec (last (first n)))
                  colright (inc (last (last n)))]
              (if (near-symbol? rowabove
                                colleft rowbelow colright arr2d n)
                (conj parts (coords->digits n arr2d))
                parts)))
          []
          ncoll))

(defn find-nums
  "iterate through matrix and collect all num sequences into a vec of vecs.
   This relies on atoms and for-loops and I feel like a bad FP'er for doing it :D"
  [arr2d]
  (let [a (to-array-2d arr2d)
        founddigits (atom [])
        foundnums (atom [])]
    (doall (for [r (range (alength a))]
             (doall
              (for [c (range (alength (aget a r)))]
                (when (Character/isDigit (aget a r c))
                  (when (or (not= (last (last @founddigits)) (dec c))
                            (not= (first (last @founddigits)) r))
                          ; not contiguous, so empty founddigits and add it to foundnums
                    (let [n @founddigits]
                      (when (seq n) ; if found nums isn't empty
                        (swap! foundnums conj n)))
                    (reset! founddigits []))
                          ; otherwise just append it to founddigits
                  (swap! founddigits conj [r c]))))))
    ; and copy the last remaining number set
    (swap! foundnums conj @founddigits)
    @foundnums))

(defn day03-1 []
  (let [data-file (slurp (io/resource "day03.txt"))
        lines (split-lines data-file)
        arr (mapv vec lines)
        ar2d (to-array-2d arr)
        foundnums (find-nums arr)]
    (apply + (find-parts foundnums ar2d))))

(defn getratio [gcoord]
  (let [matching (filter (fn [fg]
                           (= gcoord (:gearloc fg))) @found-gears)]
    (apply * (mapv :num matching))))

(defn getratios []
  (let [dupegears (mapv (fn [[g c]]
                          g)
                        (filter (fn [[g c]]
                                  (> c 1))
                                (frequencies (mapv :gearloc @found-gears))))]
    (mapv getratio dupegears)))

(defn day03-2 []
  (reset! found-gears [])
  (let [data-file (slurp (io/resource "day03.txt"))
        lines (split-lines data-file)
        arr (mapv vec lines)
        ar2d (to-array-2d arr)
        foundnums (find-nums arr)]
    (find-parts foundnums ar2d))
  (apply + (getratios)))

(comment
  (day03-1)

  (day03-2)
  #_endc)