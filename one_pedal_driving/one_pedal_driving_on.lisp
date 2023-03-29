; This script provides a way to control both accel and regen with a single throttle
; input, without creating a regen deadband at zero speed. This approach is often known
; as one pedal driving and is the default throttle response for most electric vehicles.

;1-Calculate max braking and accel current for the current speed
;2-Read throttle(ADC 0)
;3-Truncate the throttle to ensure values between 0.0 1.0
;4-Mapping curve- linear interpolation using regen and current limits
;5-Applies a curve for throttle
;6-Aplies a step-ramp from a setpoint to a desired value
;7-Set-current value (0.0 1.0)

(define speed_max      100.0) ; km/h. Set to about max vehicle speed
(define throttle_exp     1.2) ; De-linearization factor. 1.0 means fully linear
(define throttle_start  0.57) ; ADC voltage at 0% throttle
(define throttle_end    2.75) ; ADC voltage at 100% throttle
(define throttle_min     0.5) ; Min throttle voltage (wiring failure threshold)
(define throttle_max    2.95) ; Max throttle voltage (wiring failure threshold)
(define I_regen_strength 3.0) ; This factor increases or decreases the regen braking torque
(define time_sleep      0.01) ; Loop period. Set at 100Hz
(define speed            0.0) ; Wheel speed
(define setpoint_speed   0.0) ; Ramped speed


; Update values from mcconfig for OPD algorithm

(defun update_values ()
(progn
    (define Imotor_max         (conf-get 'l-current-max))
    (define Imotor_regen_max   (conf-get 'l-current-min))
    (define Ibatt_max          (conf-get 'l-in-current-max))
    (define Ibatt_regen_max    (conf-get 'l-in-current-min))
))

(conf-set 'app-to-use 0); Set to no APP usage

; Define maximum regen as a function of speed

(defun Imotor_derating_max(m)
(* (/ Ibatt_max (+ ( abs (/ m speed_max) ) 0.01)) (/ 2 (let k (sqrt 3)))) ; I motor max dynamic derating
)

(defun Imotor_regen_derating_max (m)
(* (/ Ibatt_regen_max (+ ( abs (/ m speed_max) ) 0.01)) (/ 2 (let k (sqrt 3))))
)

(defun Imotor_max_limit (m)
( if (< Imotor_max (Imotor_derating_max m)) Imotor_max (Imotor_derating_max m))
)

(defun Imotor_min_limit (m)
( if (> Imotor_regen_max (Imotor_regen_derating_max m)) Imotor_regen_max (Imotor_regen_derating_max m))
)

; Define throttle outputs range

(defun Thr_out_min (m)
(* (/ ( Imotor_min_limit m) Imotor_max) (* I_regen_strength (/ m speed_max)))
)

(defun Thr_out_max (m)
(/ ( Imotor_max_limit m) Imotor_max)
)

; Define torque command as a function of throttle position and speed

(defun I_command (throttle m)
(+ (Thr_out_min m) (* (pow throttle throttle_exp) (- (Thr_out_max m) (Thr_out_min m))))
)

; Normalize adc throttle readings, interpolation

(defun utils_map(x in_min in_max out_min out_max)
(/ (* (- x in_min) (- out_max out_min)) (+ (- in_max in_min) out_min))
)

; truncate throttle readings

(defun utils_truncate (pwr min max)
 (progn
  (cond ((> pwr max) (setvar 'Throttle_normalized max))
        ((< pwr min) (setvar 'Throttle_normalized min))
  )
 )
)

; Apply a step-towards to a desired value from a setpoint

(defun step-towards (val goal step)
    (cond
        ((< val goal)
            (if (< (+ val step) goal)
                (+ val step)
                goal
        ))

        ((> val goal)
            (if (> (- val step) goal)
                (- val step)
                goal
        ))

        (t val)
    )
)

(loopwhile t
    (progn
         (update_values)
         (def throttle_volts (get-adc 0))
         (if(or (> throttle_volts throttle_max) (< throttle_volts throttle_min) ) 
           (setvar 'Current_commanded 0.0)
           (progn
                (def throttle_linear_mapping(utils_map throttle_volts throttle_start throttle_end 0.0 1.0))
                (setvar 'Throttle_normalized(throttle-curve throttle_linear_mapping 0 0 2))
                (utils_truncate Throttle_normalized 0.0 1.0)
                (setvar 'speed (* 3.6 (get-speed))) ; converts the speed value from m/s to km/hr
                (setvar 'setpoint_speed (step-towards setpoint_speed speed 1.0)) ; filter speed signal with a ramp
                (def Current_commanded( I_command Throttle_normalized setpoint_speed))
           )
         )
         (if (< Current_commanded 0.0) (set-brake-rel (abs Current_commanded)) (set-current-rel Current_commanded))
         (sleep time_sleep)
    )
)  
    
