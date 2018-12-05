;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
(load (merge-pathnames "program.lisp" *load-truename*))

(clear-all)

(define-model jameson

;; Model parameters
(sgp :v t :esc nil :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil :show-focus t :trace-detail low)

;; Chunk types definitions
(chunk-type position pos-x pos-y)
(chunk-type environment jameson-position oldest-position middle-position newest-position
state jameson-encoded projectile-encoded)
(chunk-type estimation dx dy y action)

;; Declarative memory initialization
(add-dm
  (goal isa environment state start)
	(start)
	(attending) (encoding-jameson) (encoding-projectile) (take-action)
	(estimating) (done))

;; Environment perception productions

; Detects an object in the environment
; and moves the attention of Jameson to the object
(P attend-object
		=goal>
			isa         			environment
			state							start
		=visual-location>
			screen-x					=x
			screen-y					=y
		?visual>
			state       			free
		?imaginal>
			state							free
==>
		=goal>
			state       			attending
		+visual>
			cmd         			move-attention
			screen-pos  			=visual-location
		+imaginal>
			isa								position
			pos-x							=x
			pos-y							=y)


; Encodes the position where Jameson perceives himself
(P encode-jameson
		=goal>
			isa								environment
			state							attending
		=visual>
		  value							"J"
		?imaginal>
			buffer						full
==>
		=goal>
			state							encoding-jameson
		+visual-location>
      :attended    			nil)

; Encodes the position of an attended projectile
(P encode-projectile
		=goal>
			isa								environment
			state							attending
		=visual>
		- value							"J"
		?imaginal>
			buffer						full
==>
   =goal>
      state       retrieving
      tracked        =letter
)

; Adds the position where Jameson perceives himself at
(P add-position-jameson
		=goal>
			isa								environment
			state							encoding-jameson
		?imaginal>
			buffer						full
		=imaginal>
==>
		=goal>
			state							start
			jameson-position	=imaginal
			jameson-encoded		done)

; Adds a position to the projectile's trajectory when the
; positions for the previous 2 or 3 timesteps are known
(P add-position-full-trajectory
		=goal>
			isa								environment
			state							encoding-projectile
			middle-position		=middle-position
			newest-position		=newest-position
		?imaginal>
			buffer						full
		=imaginal>
==>
		=goal>
			state							start
			projectile-encoded	done
			oldest-position		=middle-position
			middle-position		=newest-position
			newest-position		=imaginal)

; Adds a position to the projectile's trajectory when the
; position for previous timestep is known
(P add-position-partial-trajectory
		=goal>
			isa								environment
			state							encoding-projectile
			middle-position		nil
			newest-position		=newest-position
		?imaginal>
			buffer						full
		=imaginal>
==>
		=goal>
			state							start
			projectile-encoded	done
			middle-position		=newest-position
			newest-position		=imaginal)

; Adds a position to the projectile's trajectory when no
; previous positions are known
(P add-position-no-trajectory
		=goal>
			isa								environment
			state							encoding-projectile
			newest-position		nil
		?imaginal>
			buffer						full
		=imaginal>
==>
		=goal>
			state							start
			projectile-encoded	done
			newest-position		=imaginal)

; Stops the model from progressing if it does not have
; enough information to guess the behavior of the projectile
(P cant-take-action
		=goal>
			isa         			environment
			state							start
			jameson-encoded		done
			projectile-encoded	done
		  oldest-position		nil
		?manual>
			state							free
==>
		=goal>
			jameson-encoded		nil
			projectile-encoded	nil
		+manual>
  		cmd								press-key
  		key								"s"
		+imaginal>)

; Allows the model to progress if it has enough information
; to guess the behavior of the projectile
(P can-take-action
		=goal>
			isa         			environment
			state							start
			jameson-encoded		done
			projectile-encoded	done
		- oldest-position		nil
==>
		=goal>
			jameson-encoded		nil
			projectile-encoded	nil
			state							estimating
		+imaginal>)


;; TODO Trajectory estimation productions
(P match-trajectory
		=goal>
			isa         			environment
			state							estimating
		?manual>
			state							free
==>
		!output!						"Estimating trajectory"
		=goal>
			state							nil
			jameson-position	nil
		+manual>
  		cmd								press-key
  		key								"s")

;; Information retireval productions
;; Not tested

(P remmeber-situation
    =goal>
     ISA    goal
     state     retrieving
    =retireval>
     ISA    estimation
     action =act
    ?manual>
     state free
   ==>
    =goal>
      state nil
    +manual>
     cmd press-key
     key =act
     
    @retrieval>)

(P cant-remember-up
    =goal>
      ISA goal
      state retrieving
    = y (+ pos-y dy)
    <= dy 0
    ?retrieval>
      buffer failure
    ?manual>
      state free
   ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key "u")

(P cant-remember-down
    =goal>
      ISA goal
      state retrieving
    = y (+ pos-y dy)
    > dy 0
    ?retrieval>
      buffer failure
    ?manual>
      state free
   ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key "d")

(P cant-remember-stay
    =goal>
      ISA goal
      state retrieving
      - y  (+ pos-y dy)
    ?retrieval>
      buffer failure
    ?manual>
      state free
   ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key "s")    
=======
		=goal>
			state							encoding-projectile
		+visual-location>
      :attended    			nil)


(set-all-base-levels 100000 -1000)
(goal-focus goal)
)