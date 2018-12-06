;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
(load (merge-pathnames "program.lisp" *load-truename*))

(clear-all)

(define-model jameson

;; Model parameters
(sgp :v t :esc nil :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil :show-focus t :trace-detail low)

;; Chunk types definitions
(chunk-type position pos-x pos-y)
(chunk-type environment
  jameson-position oldest-position middle-position newest-position
  state jameson-encoded projectile-encoded)
(chunk-type estimate dx dy y action)

;; Declarative memory initialization
(add-dm
  (goal isa environment state start)
  (start)

  ; Environment perception states
  (attending) (encoding-jameson) (encoding-projectile) (done) (take-action)

  ; Environment interpretation states
  (estimating)
  
  ; Learning states
  (retrieving))

;; Environment perception productions

; Detects an object in the environment
; and moves the attention of Jameson to the object
(P attend-object
    =goal>
      isa                    environment
      state                  start
    =visual-location>
      screen-x               =x
      screen-y               =y
    ?visual>
      state                  free
    ?imaginal>
      state                  free
==>
    =goal>
      state                  attending
    +visual>
      cmd                    move-attention
      screen-pos             =visual-location
    +imaginal>
      isa                    position
      pos-x                  =x
      pos-y                  =y)

; Encodes the position where Jameson perceives himself
(P encode-jameson
    =goal>
      isa                    environment
      state                  attending
    =visual>
      value                  "J"
    ?imaginal>
      buffer                 full
==>
    =goal>
      state                  encoding-jameson
    +visual-location>
      :attended              nil)

; Encodes the position of an attended projectile
(P encode-projectile
    =goal>
      isa                    environment
      state                  attending
    =visual>
    - value                  "J"
    ?imaginal>
      buffer                 full
==>
    =goal>
      state                  encoding-projectile
    +visual-location>
      :attended              nil)

; Adds the position where Jameson perceives himself at
(P add-position-jameson
    =goal>
      isa                    environment
      state                  encoding-jameson
    ?imaginal>
      buffer                 full
    =imaginal>
==>
    =goal>
      state                  start
      jameson-position       =imaginal
      jameson-encoded        done)

; Adds a position to the projectile's trajectory when the
; positions for the previous 2 or 3 timesteps are known
(P add-position-full-trajectory
    =goal>
      isa                    environment
      state                  encoding-projectile
      middle-position        =middle-position
      newest-position        =newest-position
    ?imaginal>
      buffer                 full
    =imaginal>
==>
    =goal>
      state                  start
      projectile-encoded     done
      oldest-position        =middle-position
      middle-position        =newest-position
      newest-position        =imaginal)

; Adds a position to the projectile's trajectory when the
; position for previous timestep is known
(P add-position-partial-trajectory
    =goal>
      isa                    environment
      state                  encoding-projectile
      middle-position        nil
      newest-position        =newest-position
    ?imaginal>
      buffer                 full
    =imaginal>
==>
    =goal>
      state                  start
      projectile-encoded     done
      middle-position        =newest-position
      newest-position        =imaginal)

; Adds a position to the projectile's trajectory when no
; previous positions are known
(P add-position-no-trajectory
    =goal>
      isa                    environment
      state                  encoding-projectile
      newest-position        nil
    ?imaginal>
      buffer                 full
    =imaginal>
==>
    =goal>
      state                  start
      projectile-encoded     done
      newest-position        =imaginal)

; Stops the model from progressing if it does not have
; enough information to guess the behavior of the projectile
(P cant-take-action
    =goal>
      isa                    environment
      state                  start
      jameson-encoded        done
      projectile-encoded     done
      oldest-position        nil
    ?manual>
      state                  free
==>
    =goal>
      jameson-encoded        nil
      projectile-encoded     nil
    +manual>
      cmd                    press-key
      key                    "s"
    +imaginal>)

; Allows the model to progress if it has enough information
; to guess the behavior of the projectile
(P can-take-action
    =goal>
      isa                    environment
      state                  start
      jameson-encoded        done
      projectile-encoded     done
    - oldest-position        nil
==>
    =goal>
      jameson-encoded        nil
      projectile-encoded     nil
      state                  estimating
    +imaginal>)


;; Environment interpretation productions
;; NOTE: This is the section where the estimation made by the
;;       productions should take into account more information
;;       about the environment if we want to learn more
;;       efficiently or support more use cases (e.g. parabolic
;;       trajectories)

; Interpret the environment and extract the relevant information
; that should guide the decision about where to move next
(P interpret-environment
    =goal>
      isa                    environment
      state                  estimating
      jameson-position       =j
      middle-position        =mp
      newest-position        =np
    !bind!  =np-x  (chunk-slot-value-fct =np 'pos-x)
    !bind!  =np-y  (chunk-slot-value-fct =np 'pos-y)
    !bind!  =mp-x  (chunk-slot-value-fct =mp 'pos-x)
    !bind!  =mp-y  (chunk-slot-value-fct =mp 'pos-y)
    !bind!  =dx    (- =np-x =mp-x)
    !bind!  =dy    (- =np-y =mp-y)
    !bind!  =j-y   (chunk-slot-value-fct =j 'pos-y)
==>
    =goal>
      state                  retrieving
    +retrieval>
      isa                    estimate
      dx                     =dx
      dy                     =dy
      y                      =j-y)


;; TODO Learning productions

  
(set-all-base-levels 100000 -1000)
(goal-focus goal)
)