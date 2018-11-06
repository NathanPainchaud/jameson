(defvar *jameson-speed-x* 0)  ; Variable that dictates how much Jameson's position along the x axis changes with each movement
(defvar *jameson-speed-y* 50) ; Variable that dictates how much Jameson's position along the y axis changes with each movement

;;; Class that defines the attributes of Jameson
(defclass jameson ()
  ((id
    :initarg :id
    :reader jameson-id)
   (x
    :initarg :x
    :accessor jameson-x)
   (y
    :initarg :y
    :accessor jameson-y)
   (speed-x
    :initarg :speed-x
    :reader jameson-speed-x)
   (speed-y
    :initarg :speed-y
    :reader jameson-speed-y))

   (:default-initargs
      :id "J"
      :x 250
      :y 150
      :speed-x *jameson-speed-x*
      :speed-y *jameson-speed-y*))

;; Method that moves Jameson up along the vertical axis
(defmethod move-jameson-up ((j jameson))
  (setf (jameson-y j) (- (jameson-y j) (jameson-speed-y j))))

;; Method that moves Jameson down along the vertical axis
(defmethod move-jameson-down ((j jameson))
  (setf (jameson-y j) (+ (jameson-y j) (jameson-speed-y j))))

;; Method that moves Jameson according to the decision taken by the ACT-R model
(defmethod move-jameson-on-decision ((j jameson) move-decision)
  (cond ((equal move-decision "u") (move-jameson-up j))
        ((equal move-decision "d") (move-jameson-down j))
        ((equal move-decision "s") nil)))

;; Method that displays Jameson in the experiment's window
(defmethod present-jameson ((j jameson))
  (add-text-to-exp-window :text (jameson-id j) :x (jameson-x j) :y (jameson-y j)))

;; Method that evaluates if, after the projectiles moved during the last timestep,
;; any projectile overlapped with Jameson's last position
(defmethod is-jameson-hit ((j jameson) projectiles)
  (if (null projectiles)
    nil
    (if (and (= (jameson-x j) (projectile-x (first projectiles)))
             (= (jameson-y j) (projectile-y (first projectiles))))
        t
        (is-jameson-hit j (rest projectiles)))))
