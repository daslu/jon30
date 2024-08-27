(ns jon30.vpp.utils
  (:require [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as fun]
            [scicloj.hanamicloth.v1.api :as haclo]
            [scicloj.hanamicloth.v1.plotlycloth :as ploclo]
            [tablecloth.api :as tc]
            [fastmath.interpolation :as interp]
            [clojure.math :as math]))

(def knots-to-mp 0.5144)

(def stl
  [[0 []]
   [0 [1.1 1.1]]
   [0 [2.8 1.1]]
   [0 [2.8 1.1 1.1 1.1]]
   [0 [3 1 1 1 1 1]]
   [0 [3 1 3 1 1 1 1 1]]
   [0 [7 1 1 1 1 1]]])


(def lab
  [#"$V_B$ (knots)",
   #"Heel $\\phi$ ($^\\circ$)",
   #"Leeway $\\gamma$ ($^\\circ$)",
   #"Flat",
   #"RED"])

(def cols
  ["C0", "C1", "C2", "C3", "C4", "C5", "C6"])

(defn data-path [filename]
  (str "Python-VPP-data/" filename))

(defn file-rows [fname]
  (-> (format "%s.dat" fname)
      data-path
      (format fname)
      (tc/dataset {:file-type :csv
                   :header-row? false})
      tc/rows))

(defn build-interp-fn [fname
                       {:keys [i kind]
                        :or {i 1
                             kind :linear}}]
  (->> [0 i]
       (map (file-rows fname))
       (apply interp/interpolation kind)))


(defn degrees->radians [deg]
  (-> deg
      (/ 180.0)
      (* math/PI)))
