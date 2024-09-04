(ns jon30.vpp.aero
  (:require [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as fun]
            [clojure.math :as math]
            [jon30.vpp.utils :as utils]))

(defn measure-sails [{:as aero
                      :keys [sails]}]
  (-> aero
      (assoc :fractionality 1
             :b2 0)
      ((partial reduce (fn [aero [{:keys [sail-type
                                          P P_r BAD roach CE
                                          I IG IG_r
                                          LPG_r J HBI]}]]
                         (case sail-type
                           :main (-> aero
                                     (update :fractionality
                                             #(/ % (+ P BAD)))
                                     (assoc :b1 (+ P_r BAD)
                                            :roach roach
                                            :tf (+ (* (+ (* 0.16
                                                            (- CE 0.024)
                                                            (/ P))
                                                         0.94)
                                                      P)
                                                   BAD)))

                           :jib (-> aero
                                    (update :fractionality
                                            #(* % IG_r))
                                    (assoc :b2 (* I IG_r (/ IG))
                                           :overlap LPG_r J
                                           :HBI HBI))
                           ;; else
                           aero)))
       sails)
      (update (fn [{:as aero
                    :keys [roach fractionality overlap
                           b1 b2 tf HBI]}]
                (-> aero
                    (assoc :eff-spann-corr (+ 1.1
                                              (* 0.08 (- roach 0.2))
                                              (* 0.5 (+ 0.68
                                                        (* 0.31 fractionality)
                                                        (* 0.0075 overlap)
                                                        (- 1.1))))
                           :b (max b1 b2)
                           :heff-height-max-spi (max (+ tf HBI)
                                                     0)))))))

(defn measure-windage [{:as aero
                        :keys [yacht]}]
  (let [{:keys [boa loa ff fa]} yacht]
    (-> aero
        (assoc :boa boa
               :loa loa
               :fbav (+ (* 0.625 ff)
                        (* 0.325 fa))))))

(defn ->aero [{:as params
               :keys [yacht rho mu]
               :or {rho 1.225
                    mu 0.0000181}}]
  (let [sails (->> yacht :sails (take 2))]
    (-> params
        (assoc
         ;; physical params
         :flat 1
         :reef 1
         :ftj 1
         :rfm 1
         ;; set sails and measure what is need once
         :sails sails
         :up (-> sails second :up)
         ;; coeffs interp function
         :fcdmult (utils/build-interp-fn "fcdmult")
         :kheff (utils/build-interp-fn "kheff"))
        measure-sails
        measure-windage)))

(defn get-Aref [{:as aero
                 :keys [fbav boa loa]}
                awa]
  (let [d (* 0.5
             (- 1 (math/cos (* 0.5 (utils/degrees->radians awa)))))]
    (* fbav
       (+ (* (- 1 d)
             boa)
          (* d loa)))))

(defn get-RW [{:as aero
               :keys [rho aws]}
              awa]
  (->> (* 0.5
          rho
          (math/pow aws 2)
          (get-Aref aero awa)
          0.816
          (math/cos (utils/degrees->radians awa)))))

(defn heff [aero awa])

(defn fcdmult [aero flat])

(defn get-coeffs [{:as aero
                   :keys [sails awa area flat]}]
  (let [kpp  (->> sails
                  (map (fn [{:keys [cl area bk kp]}]
                         (* (math/pow (cl awa) 2) area bk kp)))
                  (reduce +))
        cl (/ (->> sails
                   (map (fn [{:keys [cl area bk]}]
                          (* (cl awa) area bk)))
                   (reduce +))
              area)
        cd (/ (->> sails
                   (map (fn [{:keys [cd area bk]}]
                          (* (cd awa) area bk)))
                   (reduce +))
              area)
        divisor-1 (* area (math/pow cl 2))
        divisor-2 (* math/PI (math/pow (heff aero awa) 2))
        CE (+ (if (pos? divisor-1)
                (/ kpp divisor-1)
                0)
              (if (pos? divisor-2)
                (/ area divisor-2)
                0))
        fcdj (->> sails
                  (filter (fn [{:keys [sail-type]}]
                            (= sail-type :jib)))
                  (map (fn [sail]
                         (* (:bk sail)
                            ((:cd sail) awa)
                            (:area sail)
                            (/ (* cd area)))))
                  (reduce +))]
    (-> aero
        (assoc :kpp kpp
               :CE CE
               :cd (+ (* cd
                         (+ (* flat (fcdmult aero flat) fcdj)
                            (- 1 fcdj)))
                      (* CE
                         (math/pow cl 2)
                         (math/pow flat 2)
                         (fcdmult aero flat)))
               :cl (* flat cl)))))

(defn compute-forces [aero]
  (-> aero
      get-coeffs
      ((fn [aero]
         (-> aero
             (assoc :lift (* 0.5 tho (math/pow aws 2) area cl)
                    :drag (+ (* 0.5 tho (math/pow aws 2) area cd)
                             (get-Rw aero awa))))))
      ((fn [{:as aero
             :keys [lift drag awa phi]}]
         (-> aero
             (assoc :Fx (- (* lift (math/sin awa))
                           (* drag (math/cos awa)))
                    :Fy (+ (* lift (math/cos awa))
                           (* drag (math/sin awa))))
             (update :Fy #(* % (utils/degrees->radians phi))))))
      ((fn [{:as aero
             :keys [Fy]}]
         (-> aero
             (assoc :Mx (* Fy (vce aero))))))))



(defn update-aero [aero
                   {:as params
                    :keys [tws twa flat RED]}]
  (-> aero
      (merge params)
      (update :vb #(max % 0))
      (update :phi #(max % 0))
      (assoc :ftj (max (- RED 1) 0)
             :frm (min RED 1))
      measure-sails
      update-wind-triangle
      area
      compute-forces))
