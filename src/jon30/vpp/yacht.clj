(ns jon30.vpp.yacht
  (:require [clojure.math :as math]
            [jon30.vpp.utils :as utils]
            [charred.api :as charred]
            [fastmath.interpolation :as interp]))

(defn ->appendage [{:as params
                    :keys [appendage-type chord area span vol ce Ksff]
                    :or {Ksff (constantly 1)}}]
  (let [Ar (/ span area)
        dclda (/ (* 2 math/PI)
                 (+ 1.5 Ar))]
    (assoc params
           :wsa (* 2 area)
           :Ar Ar
           :dclda dclda
           :cla (* dclda area)
           :teff (* 1.8 span)
           :interp-cr (case appendage-type
                        :keel (utils/build-interp-fn "rrk")
                        :bulb (utils/build-interp-fn "rrk" {:i 2})
                        ;; else
                        (constantly 0)))))

(defn appendage-cl [{:keys [dclda]} leeway]
  (* dclda
     (utils/degrees->radians leeway)))

(defn appendage-cr [{:keys [interp-cr]} fn]
  (interp-cr (max 0 (min fn 0.6))))

(defn appendage-Ksff [{:keys [Ksff]} phi]
  (Ksff phi))

(defn print-appendage [{:keys [cu chord span wsa ce]}]
  (println "Chord root : " cu)
  (println "Chord tip : " cu)
  (println "Chord avrg : " chord)
  (println "Span : " span)
  (println "WSA : " wsa)
  (println "CE : " ce))

(defn keel-or-rudder-prep [{:as params
                            :keys [cu cl span]
                            :or {cu 1 cl 1 span 0}}]
  (let [chord (* 0.5 (+ cu cl))]
    (-> params
        (assoc :chord chord
               :area (* chord span)
               :ce (* (- span)
                      (/ (+ cu (* 2 cl))
                         (* 3 (+ cl cu))))))))

(defn ->keel [{:as params
               :keys [cu cl span]
               :or {cu 1 cl 1 span 0}}]
  (-> params
      keel-or-rudder-prep
      (#(assoc :appendage-type :keel
               :cof 1.31 ;  correciton coeff for t/c 20%
               :vol (* 0.666 (:chord %) 1.2 (:span %))))
      ->appendage))

(defn ->rudder [{:as params
                 :keys [cu cl span]
                 :or {cu 1 cl 1 span 0}}]
  (-> params
      keel-or-rudder-prep
      (#(assoc :appendage-type :rudder
               :cof 1.21 ;  correciton coeff for t/c 10%
               :vol (* 0.666 (:chord %) 1.1 (:span %))
               :Ksff (fn [phi]
                       (if (<= phi 30)
                         (+ 1 (* 0.5 (- 1 (math/cos (/ phi (* 30 math/PI))))))
                         1))))
      ->appendage))

(defn ->bulb [{:as params
               :keys [chord area vol cg]}]
  (-> params
      (assoc :span 0
             :ce cg
             :cof 1.5)
      ->appendage))

(defn build-rm-interp []
  (->> ["Heel" "GZ"]
       (map (-> "righting_moment.json"
                utils/data-path
                slurp
                charred/read-json))
       ;; Note that Fastmath linear interpolation extrapolates
       ;; when `x` is outside of the domain, which is what
       ;; the Python code also defined explicitly in this case.
       (apply interp/interpolation "linear")))

(defn ->yacht
  "
  yacht-name : Name of particular design
  lwl : waterline length (m)
  vol : volume of canoe body (m^3)
  bwl : waterline beam (m)
  tc : Canoe body draft (m)
  wsa : Wetted surface area (m^2)
  tmax : Maximum draft of yacht (m)
  amax  : Max section area (m^2)
  mass : total mass of the yacht (kg)
  app : appendages (Appendages object as list, i.e [Keel(...)] )"
  [{:as params
    :keys [yacht-name, l, vol, bwl, tc, wsa, tmax,
           amax, mass, loa, boa, ff, fa, app, sails]
    :or {app []
         sails []}}]
  (let [bmax (* 1.4 bwl)
        area-proj (* l tc 0.666)]
    (-> params
        (assoc :g 9.81
               :bmax bmax
               :bdwt 89 ; standard crew weight
               :Rm4 (* 0.43 tmax)
               ;; standard crew weight
               :cw (* 25.8 (math/pow l 1.4262))
               :carm (* 0.8 bmax) ; must be average of rail where crew sits
               ;; rough estimage of projected area of the hull
               :area-proj area-proj
               :cla (* area-proj 2 math/PI / (1 + (/ (* 0.5 area-proj) tc)))
               :teff (* 2.07 tc)
               ;; appendages
               :appendages app
               :sails sails
               ;; righting moment interpolation function
               :interp-rm (build-rm-interp)))))

(defn update-yacht [{:as yacht
                     :keys [l vol bwl tc]}]
  (-> yacht
      (assoc :lsm l
             :lvr (/ l (math/pow vol 1/3))
             :btr (/ bwl tc))))

(defn measure [yacht]
  (-> yacht
      (select-keys [:lsm :lvr :btr])))

(defn get-Rmh [{:keys [interp-rm mass g]}
               phi]
  (* (interp-rm phi)
     mass
     g))

(defn get-RmC [{:keys [carm cw bmax bdwt]}
               phi]
  (-> (* carm
         (+ (* cw (* 0.7 bmax bdwt)))
         (math/cos (utils/degrees->radians phi)))
      (cond-> (<= phi 7.5)
        (* 0.5
           (- 1 (math/cos (/ (max 0 (- phi 2.5))
                             (* 5 math/PI))))))))
