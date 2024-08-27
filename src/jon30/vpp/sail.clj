(ns jon30.vpp.sail
  (:require [jon30.vpp.utils :as utils]
            [fastmath.interpolation :as interp]
            [clojure.math :as math]))

(defn build-interp-fns [fname]
  (let [rows (utils/file-rows fname)]
    {:interp-cd (->> [1 2]
                     (map rows)
                     (apply interp/interpolation :linear))
     :interp-cl (->> [1 3]
                     (map rows)
                     (apply interp/interpolation :linear))}))

(defn ->sail [{:as params
               :keys [sail-name sail-type area vce up]
               :or {up true}}]
  (-> params
      (merge (build-interp-fns sail-type)
             {:bk 1})))

(defn cl [sail awa]
  (-> awa
      (min 180)
      (max 0)
      ((:interp-cl sail))))

(defn cd [sail awa]
  (-> awa
      (min 180)
      (max 0)
      ((:interp-cd sail))))

(defn measure-main [{:as params
                     :keys [P roach BAD area0]}
                    {:keys [rfm ftj]
                     :or {rfm 1 ftj 1}}]
  (let [P_r (* P rfm)]
    (-> params
        (assoc :P_r P_r
               :vce (+ (* P_r 1/3 (+ 1 roach))
                       BAD)
               :area (* area0 (math/pow rfm 2))
               :CE 1))))

(defn ->main [{:as params
               :keys [sail-name P E roach BAD]}]
  (-> params
      (assoc :sail-type :main
             :area0 (* P E (+ 1 roach))
             ;; probably unnecessary
             ;; (copied from Python code):
             :vce (+ (* P 1/3 (+ 1 roach))
                     BAD))
      ->sail
      measure-main))

(defn measure-jib [{:as params
                    :keys [LPG IG I J]}
                   {:keys [rfm fjt]
                    :or {rfm 1 fjt 1}}]
  (let [LPG_r (* LPG fjt)]
    (-> params
        (assoc :type :jib
               :LPG_r LPG_r
               :IG_r (* IG fjt)
               :area (* 0.5 I (max J LPG_r))))))

(defn ->jib [{:as params
              :keys [sail-name I J LPG HBI]}]
  (-> params
      (assoc :IG I
             :area (* 0.5 I (max J LPG))
             :vce (+ (/ I 3.0) HBI))
      ->sail
      measure-jib))

(defn ->kite [{:as params
               :keys [sail-name area vce]}]
  (-> params
      (assoc :type :kite)
      ->sail))
