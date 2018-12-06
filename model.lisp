;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
(load (merge-pathnames "program.lisp" *load-truename*))

(clear-all)

(define-model jameson

;; Model parameters
(sgp :v t :esc nil :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil :show-focus t :trace-detail low)

;; Chunk types definitions
(chunk-type position pos-x pos-y)
(chunk-type estimate position direction action)
(chunk-type environment
  jameson-position oldest-position middle-position newest-position
  jameson-encoded projectile-encoded
  projectile-position projectile-direction
  action result state)


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
    !bind!  =np-y  (chunk-slot-value-fct =np 'pos-y)
    !bind!  =mp-y  (chunk-slot-value-fct =mp 'pos-y)
    !bind!  =j-y   (chunk-slot-value-fct =j 'pos-y)
    !bind!  =position   (cond
                          ((equal =np-y =j-y) "equal")
                          ((> =np-y =j-y) "lower")
                          ((< =np-y =j-y) "higher"))
    !bind!  =direction  (cond
                          ((equal =np-y =mp-y) "level")
                          ((> =np-y =mp-y) "down")
                          ((< =np-y =mp-y) "up"))
==>
    =goal>
      state                  retrieving
      projectile-position    =position
      projectile-direction   =direction
    +retrieval>
      isa                    estimate
      position               =position
      direction              =direction)


;; New environment heuristics productions

; Provides a heuristic for when it should be a good
; decision not to move, if the environment is not known
(P cant-remember-stay
    =goal>
      isa                    environment
      state                  retrieving
    - projectile-position    "equal"
    - projectile-direction   "level"
    ?retrieval>
      buffer                 failure
    ?manual>
      state free
==>
    =goal>
      state                  start
    +manual>
      cmd                    press-key
      key                    "s")

; Provides a heuristic for when it should be a good
; decision to move up, if the environment is not known
(P cant-remember-up
    =goal>
      isa                    environment
      state                  retrieving
    - projectile-position    "lower"
    - projectile-direction   "up"
    ?retrieval>
      buffer                 failure
    ?manual>
      state free
==>
    =goal>
      state                  start
    +manual>
      cmd                    press-key
      key                    "u")

; Provides a heuristic for when it should be a good
; decision to move down, if the environment is not known
(P cant-remember-down
    =goal>
      isa                    environment
      state                  retrieving
    - projectile-position    "higher"
    - projectile-direction   "down"
    ?retrieval>
      buffer                 failure
    ?manual>
      state free
==>
    =goal>
      state                  start
    +manual>
      cmd                    press-key
      key                    "d")


;; Chunk retrieval learning productions

; Executes the action usually taken when in that
; environment if the environment is known
(P remember-environment
    =goal>
      isa                    environment
      state                  retrieving
    =retrieval>
      isa                    estimate
      action                 =action
    ?manual>
      state                  free
==>
    =goal>
      state                  start
      action                 =action
    +manual>
      cmd                    press-key
      key                    =action
    @retrieval>)

; Remembers the action taken if it lead to not being hit
(P results-not-hit
    =goal
      isa
      state                  results
      projectile-position    =position
      projectile-direction   =direction
      action                 =action
      result                 "no-hit"
==>
    +goal>
      isa                    environment
      state                  start
    +imaginal>
      isa                    estimate
      projectile-position    =position
      projectile-direction   =direction
      action                 =action)

; Forgets the action taken if it lead to being hit
(P results-hit
    =goal
      isa
      state                  results
      result                 "hit"
==>
    +goal>
      isa                    environment
      state                  start)

(goal-focus goal)

;; Initialize utilities for each action production
(spp cant-remember-stay :u 10)
(spp cant-remember-up :u 10)
(spp cant-remember-down :u 10)

;; Trigger the reward at the end of a trial
(when *learning*
  (spp results-not-hit :reward 10)
  (spp results-hit :reward 0)))