(ns advent2023.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str :refer [split-lines split]])
  (:gen-class))

(def maxcubes {:red 12 :blue 14 :green 13})

(defn get-maxes [cts]
  (reduce (fn [maxes ct]
            (println ct)
            {:red (if (contains? ct :red)
                    (max (:red ct) (:red maxes))
                    (:red maxes))
             :green (if (contains? ct :green)
                      (max (:green ct) (:green maxes))
                      (:green maxes))
             :blue (if (contains? ct :blue)
                     (max (:blue ct) (:blue maxes))
                     (:blue maxes))}) {:red 0, :green 0, :blue 0}
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