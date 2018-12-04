;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
(load (merge-pathnames "program.lisp" *load-truename*))

(clear-all)

(define-model jameson

(sgp :v t :esc nil :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil :show-focus t :trace-detail low)

  
;; PRODUCTIONS - let's try to keep them organized
;; GENERAL

(chunk-type position posx posy)
(chunk-type trajectory position1 position2 position3 course state encode-state)
(chunk-type projectile id trajectory)

(add-dm
  (goal isa trajectory)(attended)(attending)(done)(pos-encoded)
  (pos1-encoded)(pos2-encoded)(pos3-encoded)(estimation))


(P respond
   =goal>
      ISA         trajectory
      state       done
   ?manual>
     state        free
==>
   +goal>
   +manual>
      cmd         press-key
      key         "s"
)

;; VISUAL-MODULE PRODUCTION

(P attend-projectile
   =goal>
      ISA         trajectory
			state				nil
   =visual-location>
   		screen-x				=pos-x
   		screen-y				=pos-y
   ?visual>
      state       free
   ?imaginal>
   		state				free
   
==>
   +visual>
      cmd         move-attention
      screen-pos  =visual-location
   =goal>
      state       nil
   +imaginal>
   		isa					position
   		posx				=pos-x
   		posy				=pos-y
)


(P encode-trajectory-one
		=goal>
			isa						trajectory
			state					nil
			encode-state	nil
		?imaginal>
			buffer				full
		=imaginal>
		?manual>
			state					free
==>
		=goal>
			position1			=imaginal
			encode-state	pos1-encoded
   	+manual>
  		cmd       	  press-key
  		key       	  "s"
  	+imaginal>
  	
)
(P encode-trajectory-two
		=goal>
			isa						trajectory
			state					nil
			encode-state	pos1-encoded
		?imaginal>
			buffer				full
		=imaginal>
		?manual>
			state					free
==>
		=goal>
			position2			=imaginal
			encode-state	pos2-encoded
		+imaginal>
   	+manual>
  		cmd       	  press-key
  		key       	  "s"
)
(P encode-trajectory-three
		=goal>
			isa						trajectory
			encode-state	pos2-encoded
			state					nil
		?imaginal>
			buffer 				full
		=imaginal>
		?manual>
			state 				free
==>
		=goal>
			position3		  =imaginal
			encode-state  estimation
   	+manual>
  		cmd    	     press-key
  		key    	     "s"
  	+imaginal>
  	
)
(P estimate-course
		=goal>
			isa						trajectory
			encode-state	estimation		
			position1			=p1

==>
		!output!				"Estimating trajectory"
		=goal>
			encode-state	estimate
			state					estimate
		+retrieval>
			isa						position
			pos-xy				=p1
)
(P estimate-course-2
		=goal>
			isa						trajectory
			encode-state	estimation		
			state					estimation
		=retrieval>
			state					full
==>
		!output!				"Estimating trajectory"

)
			
			

(set-all-base-levels 100000 -1000)
(goal-focus goal)
)
