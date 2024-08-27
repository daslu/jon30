(ns jon30.vpp.aero
  (:require [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as fun]
            [clojure.math :as math]))

(defn ->aero [{:keys [yacht rho mu]
               :or {rho 1.225
                    mu 0.0000181}}]
  (let [sails (->> yacht :sails (take 2))]
    {;; physical params
     :rho rho
     :mu mu
     :flat 1
     :reef 1
     :ftj 1
     :rfm 1
     ;; set sails and measure what is need once
     :yacht yacht
     :sails sails
     :up (-> sails second :up)}))
