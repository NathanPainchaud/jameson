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
	jameson-position-x jameson-position-y
	oldest-position-x oldest-position-y
	middle-position-x middle-position-y 
	newest-position-x newest-position-y state)
(chunk-type estimate jameson-position-y dx dy y action state)

;; Declarative memory initialization
(add-dm
  (goal isa environment state start)
	(start)
	(attending) (encoding-jameson) (encoding-projectile) (take-action)
	(estimating) (retrieving) (done))

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
			state							encoding-projectile
		+visual-location>
      :attended    			nil)

; Adds the position where Jameson perceives himself at
(P add-position-jameson
		=goal>
			isa								environment
			state							encoding-jameson
		?imaginal>
			buffer						full
		=imaginal>
			pos-x 	=x
			pos-y 	=y
==>			
		=goal>
			state							start
			jameson-position-x	=x
			jameson-position-y	=y
			jameson-encoded		done)

; Adds a position to the projectile's trajectory when the
; positions for the previous 2 or 3 timesteps are known
(P add-position-full-trajectory
		=goal>
			isa								environment
			state							encoding-projectile
			middle-position-x		=middle-position-x
			middle-position-y		=middle-position-y
			newest-position-x		=newest-position-x
			newest-position-y		=newest-position-y

		?imaginal>
			buffer						full
		=imaginal>
			pos-x 	=x
			pos-y 	=y
==>
		=goal>
			state							start
			projectile-encoded	done
			oldest-position-x		=middle-position-x
			oldest-position-y		=middle-position-y
			middle-position-x		=newest-position-x
			middle-position-y		=newest-position-y
			newest-position-x		=x
			newest-position-y		=y)

; Adds a position to the projectile's trajectory when the
; position for previous timestep is known
(P add-position-partial-trajectory
		=goal>
			isa								environment
			state							encoding-projectile
			middle-position-x		nil
			middle-position-y		nil
			newest-position-x		=newest-position-x
			newest-position-y		=newest-position-y
		?imaginal>
			buffer						full
		=imaginal>
			pos-x 	=x
			pos-y 	=y
==>
		=goal>
			state							start
			projectile-encoded	done
			middle-position-x		=newest-position-x
			middle-position-y		=newest-position-y
			newest-position-x		=x
			newest-position-y		=y)

; Adds a position to the projectile's trajectory when no
; previous positions are known
(P add-position-no-trajectory
		=goal>
			isa								environment
			state							encoding-projectile
			newest-position-x		nil
			newest-position-y		nil
		?imaginal>
			buffer						full
		=imaginal>
			pos-x 	=x
			pos-y 	=y
==>
		=goal>
			state							start
			projectile-encoded	done
			newest-position-x		=x
			newest-position-y		=y)

; Stops the model from progressing if it does not have
; enough information to guess the behavior of the projectile
(P cant-take-action
		=goal>
			isa         			environment
			state							start
			jameson-encoded		done
			projectile-encoded	done
		  	oldest-position-x		nil
		  	oldest-position-y		nil
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
		- oldest-position-x		nil
		- oldest-position-y		nil
==>
		=goal>
			state							estimating
			jameson-encoded		nil
			projectile-encoded	nil
		+imaginal>)


;; TODO Trajectory estimation productions
(P match-trajectory
		=goal>
			isa         			environment
			state							estimating
			jameson-position-y			=j-y
			middle-position-x			=p2-x
			middle-position-y			=p2-y
			newest-position-x			=p3-x
			newest-position-y			=p3-y
		?manual>
			state							free
		
==>
		!output!						"Estimating trajectory"
		
		!bind! =deltaX (- =p3-x =p2-x)
		!bind! =deltaY (- =p3-y =p2-y)
		=goal>
			isa				estimate
			state							nil
			jameson-position-y	nil
			dx =deltaX
			dy =deltaY 
			y =j-y
					
		+manual>
			cmd								press-key
			key								"s")

;; Information retireval productions
;; Not tested

	
(set-all-base-levels 100000 -1000)
(goal-focus goal)
)