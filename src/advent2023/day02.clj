(ns advent2023.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str :refer [split-lines split]])
  (:gen-class))

(def maxcubes {:red 12 :blue 14 :green 13})

(defn get-maxes [cts]
  (reduce (fn [maxes ct]
            (merge maxes
                   (into {}
                         (for [k (keys ct) :when (contains? ct k)]
                           [k (max (get ct k) (get maxes k 0))]))))
          {:red 0, :green 0, :blue 0}
          cts))

(defn counts-from-set [st]
  (let [hand (split st #", ")]
    (into {} (merge (mapv (fn [c]
                            (let [[n col] (split c #" ")
                                  n (Integer/parseInt n)]
                              {(keyword col) n})) hand)))))

(defn game-id [ln]
  (let [[game _] (split ln #": ")
        [_ game-id] (split game #" ")]
    (Integer/parseInt game-id)))

(defn possible? [ln]
  (let [[_ results] (split ln #": ")
        sets (split results #"; ")
        counts (mapv counts-from-set sets)
        mostseen (get-maxes counts)]
    (and (>= (:red maxcubes) (:red mostseen))
         (>= (:blue maxcubes) (:blue mostseen))
         (>= (:green maxcubes) (:green mostseen)))))

(defn day02-1 []
  (let [data-file (slurp (io/resource "day02.txt"))
        lines (split-lines data-file)]
    (apply + (mapv game-id (filter possible? lines)))))

(defn powers [ln]
  (let [[_ results] (split ln #": ")
        sets (split results #"; ")
        counts (mapv counts-from-set sets)
        maxes (get-maxes counts)]
    (* (:red maxes)
       (:blue maxes)
       (:green maxes))))

(defn day02-2 []
  (let [data-file (slurp (io/resource "day02.txt"))
        lines (split-lines data-file)]
    (apply + (mapv powers lines))))

(comment

  (day02-1)
  (day02-2)

  #_endc)