(define (domain satellite)
 (:requirements :strips :durative-actions)
 (:types )
 (:constants )
 (:predicates (pointing_satellite0_star1) (pointing_satellite0_star2) (pointing_satellite0_groundstation0) (pointing_satellite0_planet3) (pointing_satellite0_phenomenon4) (pointing_satellite0_star5) (pointing_satellite0_star6) (power_avail_satellite0) (power_on_instrument0) (calibrated_instrument0) (power_on_instrument1) (have_image_planet3_image0) (have_image_star5_thermograph2) (have_image_star6_thermograph2))
 (:durative-action turn_to_satellite0_star2_star1
  :parameters ()
  :duration (= ?duration 17)
  :condition (and (at start (pointing_satellite0_star1)))
  :effect (and (at end (pointing_satellite0_star2)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_satellite0_star2_groundstation0
  :parameters ()
  :duration (= ?duration 9)
  :condition (and (at start (pointing_satellite0_groundstation0)))
  :effect (and (at end (pointing_satellite0_star2)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_satellite0_star2_planet3
  :parameters ()
  :duration (= ?duration 4)
  :condition (and (at start (pointing_satellite0_planet3)))
  :effect (and (at end (pointing_satellite0_star2)) (at start (not (pointing_satellite0_planet3)))))
 (:durative-action turn_to_satellite0_star2_phenomenon4
  :parameters ()
  :duration (= ?duration 3)
  :condition (and (at start (pointing_satellite0_phenomenon4)))
  :effect (and (at end (pointing_satellite0_star2)) (at start (not (pointing_satellite0_phenomenon4)))))
 (:durative-action turn_to_satellite0_star2_star5
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (pointing_satellite0_star5)))
  :effect (and (at end (pointing_satellite0_star2)) (at start (not (pointing_satellite0_star5)))))
 (:durative-action turn_to_satellite0_star2_star6
  :parameters ()
  :duration (= ?duration 10)
  :condition (and (at start (pointing_satellite0_star6)))
  :effect (and (at end (pointing_satellite0_star2)) (at start (not (pointing_satellite0_star6)))))
 (:durative-action turn_to_satellite0_star1_star2
  :parameters ()
  :duration (= ?duration 17)
  :condition (and (at start (pointing_satellite0_star2)))
  :effect (and (at end (pointing_satellite0_star1)) (at start (not (pointing_satellite0_star2)))))
 (:durative-action turn_to_satellite0_star1_groundstation0
  :parameters ()
  :duration (= ?duration 22)
  :condition (and (at start (pointing_satellite0_groundstation0)))
  :effect (and (at end (pointing_satellite0_star1)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_satellite0_star1_planet3
  :parameters ()
  :duration (= ?duration 20)
  :condition (and (at start (pointing_satellite0_planet3)))
  :effect (and (at end (pointing_satellite0_star1)) (at start (not (pointing_satellite0_planet3)))))
 (:durative-action turn_to_satellite0_star1_phenomenon4
  :parameters ()
  :duration (= ?duration 19)
  :condition (and (at start (pointing_satellite0_phenomenon4)))
  :effect (and (at end (pointing_satellite0_star1)) (at start (not (pointing_satellite0_phenomenon4)))))
 (:durative-action turn_to_satellite0_star1_star5
  :parameters ()
  :duration (= ?duration 15)
  :condition (and (at start (pointing_satellite0_star5)))
  :effect (and (at end (pointing_satellite0_star1)) (at start (not (pointing_satellite0_star5)))))
 (:durative-action turn_to_satellite0_star1_star6
  :parameters ()
  :duration (= ?duration 23)
  :condition (and (at start (pointing_satellite0_star6)))
  :effect (and (at end (pointing_satellite0_star1)) (at start (not (pointing_satellite0_star6)))))
 (:durative-action turn_to_satellite0_groundstation0_star2
  :parameters ()
  :duration (= ?duration 9)
  :condition (and (at start (pointing_satellite0_star2)))
  :effect (and (at end (pointing_satellite0_groundstation0)) (at start (not (pointing_satellite0_star2)))))
 (:durative-action turn_to_satellite0_groundstation0_star1
  :parameters ()
  :duration (= ?duration 22)
  :condition (and (at start (pointing_satellite0_star1)))
  :effect (and (at end (pointing_satellite0_groundstation0)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_satellite0_groundstation0_planet3
  :parameters ()
  :duration (= ?duration 11)
  :condition (and (at start (pointing_satellite0_planet3)))
  :effect (and (at end (pointing_satellite0_groundstation0)) (at start (not (pointing_satellite0_planet3)))))
 (:durative-action turn_to_satellite0_groundstation0_phenomenon4
  :parameters ()
  :duration (= ?duration 12)
  :condition (and (at start (pointing_satellite0_phenomenon4)))
  :effect (and (at end (pointing_satellite0_groundstation0)) (at start (not (pointing_satellite0_phenomenon4)))))
 (:durative-action turn_to_satellite0_groundstation0_star5
  :parameters ()
  :duration (= ?duration 7)
  :condition (and (at start (pointing_satellite0_star5)))
  :effect (and (at end (pointing_satellite0_groundstation0)) (at start (not (pointing_satellite0_star5)))))
 (:durative-action turn_to_satellite0_groundstation0_star6
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (pointing_satellite0_star6)))
  :effect (and (at end (pointing_satellite0_groundstation0)) (at start (not (pointing_satellite0_star6)))))
 (:durative-action turn_to_satellite0_planet3_star2
  :parameters ()
  :duration (= ?duration 4)
  :condition (and (at start (pointing_satellite0_star2)))
  :effect (and (at end (pointing_satellite0_planet3)) (at start (not (pointing_satellite0_star2)))))
 (:durative-action turn_to_satellite0_planet3_star1
  :parameters ()
  :duration (= ?duration 20)
  :condition (and (at start (pointing_satellite0_star1)))
  :effect (and (at end (pointing_satellite0_planet3)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_satellite0_planet3_groundstation0
  :parameters ()
  :duration (= ?duration 11)
  :condition (and (at start (pointing_satellite0_groundstation0)))
  :effect (and (at end (pointing_satellite0_planet3)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_satellite0_planet3_phenomenon4
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (pointing_satellite0_phenomenon4)))
  :effect (and (at end (pointing_satellite0_planet3)) (at start (not (pointing_satellite0_phenomenon4)))))
 (:durative-action turn_to_satellite0_planet3_star5
  :parameters ()
  :duration (= ?duration 6)
  :condition (and (at start (pointing_satellite0_star5)))
  :effect (and (at end (pointing_satellite0_planet3)) (at start (not (pointing_satellite0_star5)))))
 (:durative-action turn_to_satellite0_planet3_star6
  :parameters ()
  :duration (= ?duration 13)
  :condition (and (at start (pointing_satellite0_star6)))
  :effect (and (at end (pointing_satellite0_planet3)) (at start (not (pointing_satellite0_star6)))))
 (:durative-action turn_to_satellite0_phenomenon4_star2
  :parameters ()
  :duration (= ?duration 3)
  :condition (and (at start (pointing_satellite0_star2)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at start (not (pointing_satellite0_star2)))))
 (:durative-action turn_to_satellite0_phenomenon4_star1
  :parameters ()
  :duration (= ?duration 19)
  :condition (and (at start (pointing_satellite0_star1)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_satellite0_phenomenon4_groundstation0
  :parameters ()
  :duration (= ?duration 12)
  :condition (and (at start (pointing_satellite0_groundstation0)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_satellite0_phenomenon4_planet3
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (pointing_satellite0_planet3)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at start (not (pointing_satellite0_planet3)))))
 (:durative-action turn_to_satellite0_phenomenon4_star5
  :parameters ()
  :duration (= ?duration 5)
  :condition (and (at start (pointing_satellite0_star5)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at start (not (pointing_satellite0_star5)))))
 (:durative-action turn_to_satellite0_phenomenon4_star6
  :parameters ()
  :duration (= ?duration 13)
  :condition (and (at start (pointing_satellite0_star6)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at start (not (pointing_satellite0_star6)))))
 (:durative-action turn_to_satellite0_star5_star2
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (pointing_satellite0_star2)))
  :effect (and (at end (pointing_satellite0_star5)) (at start (not (pointing_satellite0_star2)))))
 (:durative-action turn_to_satellite0_star5_star1
  :parameters ()
  :duration (= ?duration 15)
  :condition (and (at start (pointing_satellite0_star1)))
  :effect (and (at end (pointing_satellite0_star5)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_satellite0_star5_groundstation0
  :parameters ()
  :duration (= ?duration 7)
  :condition (and (at start (pointing_satellite0_groundstation0)))
  :effect (and (at end (pointing_satellite0_star5)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_satellite0_star5_planet3
  :parameters ()
  :duration (= ?duration 6)
  :condition (and (at start (pointing_satellite0_planet3)))
  :effect (and (at end (pointing_satellite0_star5)) (at start (not (pointing_satellite0_planet3)))))
 (:durative-action turn_to_satellite0_star5_phenomenon4
  :parameters ()
  :duration (= ?duration 5)
  :condition (and (at start (pointing_satellite0_phenomenon4)))
  :effect (and (at end (pointing_satellite0_star5)) (at start (not (pointing_satellite0_phenomenon4)))))
 (:durative-action turn_to_satellite0_star5_star6
  :parameters ()
  :duration (= ?duration 9)
  :condition (and (at start (pointing_satellite0_star6)))
  :effect (and (at end (pointing_satellite0_star5)) (at start (not (pointing_satellite0_star6)))))
 (:durative-action turn_to_satellite0_star6_star2
  :parameters ()
  :duration (= ?duration 10)
  :condition (and (at start (pointing_satellite0_star2)))
  :effect (and (at end (pointing_satellite0_star6)) (at start (not (pointing_satellite0_star2)))))
 (:durative-action turn_to_satellite0_star6_star1
  :parameters ()
  :duration (= ?duration 23)
  :condition (and (at start (pointing_satellite0_star1)))
  :effect (and (at end (pointing_satellite0_star6)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_satellite0_star6_groundstation0
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (pointing_satellite0_groundstation0)))
  :effect (and (at end (pointing_satellite0_star6)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_satellite0_star6_planet3
  :parameters ()
  :duration (= ?duration 13)
  :condition (and (at start (pointing_satellite0_planet3)))
  :effect (and (at end (pointing_satellite0_star6)) (at start (not (pointing_satellite0_planet3)))))
 (:durative-action turn_to_satellite0_star6_phenomenon4
  :parameters ()
  :duration (= ?duration 13)
  :condition (and (at start (pointing_satellite0_phenomenon4)))
  :effect (and (at end (pointing_satellite0_star6)) (at start (not (pointing_satellite0_phenomenon4)))))
 (:durative-action turn_to_satellite0_star6_star5
  :parameters ()
  :duration (= ?duration 9)
  :condition (and (at start (pointing_satellite0_star5)))
  :effect (and (at end (pointing_satellite0_star6)) (at start (not (pointing_satellite0_star5)))))
 (:durative-action switch_on_instrument0_satellite0
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (power_avail_satellite0)))
  :effect (and (at end (power_on_instrument0)) (at start (not (power_avail_satellite0))) (at start (not (calibrated_instrument0)))))
 (:durative-action switch_on_instrument1_satellite0
  :parameters ()
  :duration (= ?duration 2)
  :condition (and (at start (power_avail_satellite0)))
  :effect (and (at end (power_on_instrument1)) (at start (not (power_avail_satellite0)))))
 (:durative-action switch_off_instrument0_satellite0
  :parameters ()
  :duration (= ?duration 1)
  :condition (and (at start (power_on_instrument0)))
  :effect (and (at end (power_avail_satellite0)) (at start (not (power_on_instrument0)))))
 (:durative-action switch_off_instrument1_satellite0
  :parameters ()
  :duration (= ?duration 1)
  :condition (and (at start (power_on_instrument1)))
  :effect (and (at end (power_avail_satellite0)) (at start (not (power_on_instrument1)))))
 (:durative-action calibrate_satellite0_instrument0_star1
  :parameters ()
  :duration (= ?duration 30)
  :condition (and (over all (pointing_satellite0_star1)) (over all (power_on_instrument0)))
  :effect (at end (calibrated_instrument0)))
 (:durative-action turn_to_and_calibrate_satellite0_instrument0_star2_star1
  :parameters ()
  :duration (= ?duration 30)
  :condition (and (at start (pointing_satellite0_star1)) (over all (power_on_instrument0)))
  :effect (and (at end (pointing_satellite0_star2)) (at end (calibrated_instrument0)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument0_groundstation0_star1
  :parameters ()
  :duration (= ?duration 30)
  :condition (and (at start (pointing_satellite0_star1)) (over all (power_on_instrument0)))
  :effect (and (at end (pointing_satellite0_groundstation0)) (at end (calibrated_instrument0)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument0_planet3_star1
  :parameters ()
  :duration (= ?duration 30)
  :condition (and (at start (pointing_satellite0_star1)) (over all (power_on_instrument0)))
  :effect (and (at end (pointing_satellite0_planet3)) (at end (calibrated_instrument0)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument0_phenomenon4_star1
  :parameters ()
  :duration (= ?duration 30)
  :condition (and (at start (pointing_satellite0_star1)) (over all (power_on_instrument0)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at end (calibrated_instrument0)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument0_star5_star1
  :parameters ()
  :duration (= ?duration 30)
  :condition (and (at start (pointing_satellite0_star1)) (over all (power_on_instrument0)))
  :effect (and (at end (pointing_satellite0_star5)) (at end (calibrated_instrument0)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument0_star6_star1
  :parameters ()
  :duration (= ?duration 30)
  :condition (and (at start (pointing_satellite0_star1)) (over all (power_on_instrument0)))
  :effect (and (at end (pointing_satellite0_star6)) (at end (calibrated_instrument0)) (at start (not (pointing_satellite0_star1)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument1_star2_groundstation0
  :parameters ()
  :duration (= ?duration 31)
  :condition (and (at start (pointing_satellite0_groundstation0)) (over all (power_on_instrument1)))
  :effect (and (at end (pointing_satellite0_star2)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument1_star1_groundstation0
  :parameters ()
  :duration (= ?duration 31)
  :condition (and (at start (pointing_satellite0_groundstation0)) (over all (power_on_instrument1)))
  :effect (and (at end (pointing_satellite0_star1)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument1_planet3_groundstation0
  :parameters ()
  :duration (= ?duration 31)
  :condition (and (at start (pointing_satellite0_groundstation0)) (over all (power_on_instrument1)))
  :effect (and (at end (pointing_satellite0_planet3)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument1_phenomenon4_groundstation0
  :parameters ()
  :duration (= ?duration 31)
  :condition (and (at start (pointing_satellite0_groundstation0)) (over all (power_on_instrument1)))
  :effect (and (at end (pointing_satellite0_phenomenon4)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument1_star5_groundstation0
  :parameters ()
  :duration (= ?duration 31)
  :condition (and (at start (pointing_satellite0_groundstation0)) (over all (power_on_instrument1)))
  :effect (and (at end (pointing_satellite0_star5)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action turn_to_and_calibrate_satellite0_instrument1_star6_groundstation0
  :parameters ()
  :duration (= ?duration 31)
  :condition (and (at start (pointing_satellite0_groundstation0)) (over all (power_on_instrument1)))
  :effect (and (at end (pointing_satellite0_star6)) (at start (not (pointing_satellite0_groundstation0)))))
 (:durative-action take_image_satellite0_planet3_instrument0_image0
  :parameters ()
  :duration (= ?duration 7)
  :condition (and (over all (pointing_satellite0_planet3)) (over all (power_on_instrument0)) (over all (calibrated_instrument0)))
  :effect (at end (have_image_planet3_image0)))
 (:durative-action take_image_satellite0_star5_instrument0_thermograph2
  :parameters ()
  :duration (= ?duration 7)
  :condition (and (over all (pointing_satellite0_star5)) (over all (power_on_instrument0)) (over all (calibrated_instrument0)))
  :effect (at end (have_image_star5_thermograph2)))
 (:durative-action take_image_satellite0_star6_instrument0_thermograph2
  :parameters ()
  :duration (= ?duration 7)
  :condition (and (over all (pointing_satellite0_star6)) (over all (power_on_instrument0)) (over all (calibrated_instrument0)))
  :effect (at end (have_image_star6_thermograph2)))
;; atom sets
;; action sets
;; action partitions
)
