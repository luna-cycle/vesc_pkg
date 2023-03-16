; This script provides a way to control both accel and regen with a single throttle
; input, without creating a regen deadband at zero speed. This approach is often known
; as one pedal driving and is the default throttle response for most electric vehicles.

;0-Calculate max braking current for the current speed
;1-Calculate max phase current for the current speed
;1-Read throttle(ADC 0);(done)
;5-Truncate the throttle to ensure values between 0.0 1.0 (not yet)
;2-Mapping curve- linear interpolation using regen and current limits (not yet)
;4-Applies a curve for throttle (done)
;5-Aplies a step-ramp from a setpoint to a desired value (done)
;6-Set-current value (0.0 1.0) (done)

(def throttle_ramped  0.0)
(define time_sleep    0.01)
(define adc_volts    0.0)

(conf-set 'app-to-use 0) ; Set to no APP usage

; define a function which performs a ramp for the throttle reading

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

; mapping values

(defun utils_map(x in_min in_max out_min out_max)
  (/ (* (- x in_min) (- out_max out_min)) (+ (- in_max in_min) out_min))
)

; Current output

(defun current(setpoint)
  (progn
  (set-current-rel setpoint)))

; main loop

(loopwhile t
    (progn
         (def throttle_volts (get-adc adc_volts)) ; read the throttle input
         (def throttle_linear_mapping (utils_map throttle_volts 0.56 2.75 0.0 1.0)) ; apply mapping  curve
         (def throttle_exp_correction(throttle-curve throttle_linear_mapping 0 0 2)) ; apply polynomial throttle curve
         (def throttle_ramped (step-towards throttle_ramped throttle_exp_correction 1))
         (current throttle_ramped)
        ;(def RPM (get-rpm))
        ;(def speed (get-speed))
         (sleep time_sleep)   
   ))
