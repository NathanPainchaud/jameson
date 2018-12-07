(defvar *projectile-positions-y* '(50 100 150 200 250)) ; List of possible points for projectiles along the vertical axis
(defvar *projectile-start-positions-y* '(100 150 200)) ; List of possible starting points for projectiles along the vertical axis
(defvar *projectile-position-x* 50) ; Position on th x axis at which all projectiles initially start

(defvar *projectile-speed-x* 50) ; Variable that dictates how much a projectile's position along the x axis changes at each timestep
(defvar *projectile-speed-y* 50)  ; Variable that dictates how much a projectile's position along the y axis changes at each timestep

(defclass projectile ()
  ((id
    :initarg :id
    :reader projectile-id)
   (x
    :initarg :x
    :accessor projectile-x)
   (y
    :initarg :y
    :accessor projectile-y)
   (speed-x
    :initarg :speed-x
    :reader projectile-speed-x)
   (speed-y
    :initarg :speed-y
    :accessor projectile-speed-y))

   (:default-initargs
      :id "1"
      :x *projectile-position-x*
      :y (nth (random (length *projectile-start-positions-y*)) *projectile-start-positions-y*)
      :speed-x *projectile-speed-x*
      :speed-y (let ((rand (random 3)))
                 (cond
                   ((= rand 0) 0)
                   ((= rand 1) *projectile-speed-y*)
                   ((= rand 2) (- *projectile-speed-y*))))))

;; Method that returns a new projectile, corresponding to the projectile given in parameter advancing for one timestep
(defmethod move-projectile ((p projectile))
  (let ((speed-y (if (and (/= (projectile-speed-y p) 0)
                          (or (= (projectile-y p) (first *projectile-positions-y*))
                              (= (projectile-y p) (first (last *projectile-positions-y*)))))
                     (- (projectile-speed-y p))
                     (projectile-speed-y p))))
    (make-instance 'projectile :id (projectile-id p)
                               :x (+ (projectile-x p) (projectile-speed-x p))
                               :y (+ (projectile-y p) speed-y)
                               :speed-x (projectile-speed-x p)
                               :speed-y speed-y)))

;; Method that displays a projectile in the experiment's window
(defmethod present-projectile ((p projectile))
  (add-text-to-exp-window :text (projectile-id p) :x (projectile-x p) :y (projectile-y p)))


;; Function that generates a list of randomly initialized projectiles
(defun generate-projectiles (projectiles-nb)
  (let ((projectiles '()))
    (dotimes (i projectiles-nb projectiles)
      (push (make-instance 'projectile :id (format nil "~d" i)) projectiles))))

;; Function that displays all the projectiles in a list in the experiment's window
(defun present-projectiles (projectiles)
  (mapc #'present-projectile projectiles))
