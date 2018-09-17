(in-package :refills-cram)

;; prebuild variables for prolog queries
(defvar *map* "map")
(defvar *shop* "shop")
(defvar *shelf_floor* (format nil "~a:'ShelfLayer'" *shop*))
(defvar *dm_market* "dmshop")
(defvar *shelf_system* (format nil "~a:'DMShelfSystem'" *dm_market*))
(defvar *shelf_meter* (format nil "~a:'DMShelfFrameFrontStore'" *dm_market*))
(defvar *shelf_layer* (format nil "~a:'DMShelfLayer'" *dm_market*))
(defvar *shelf_frame* (format nil "~a:'DMShelfFrame'" *dm_market*))
(defvar *shelf_floor_standing* (format nil "~a:'DMShelfLayer4TilesFront'" *dm_market*))
(defvar *shelf_floor_standing_ground* (format nil "~a:'DMShelfLayer5TilesFront'" *dm_market*))
(defvar *shelf_floor_mounting* (format nil "~a:'DMShelfLayerMountingFront'" *dm_market*))
(defvar *seperator* (format nil "~a:'DMShelfSeperator4Tiles'" *dm_market*))
(defvar *mounting_bar* (format nil "~a:'DMShelfMountingBar'" *dm_market*))
(defvar *barcode* (format nil "~a:'DMShelfLabel'" *dm_market*))
(defvar *perception_affordance* (format nil "~a:'DMShelfPerceptionAffordance'" *dm_market*))

(defvar *object_acted_on* "'http://knowrob.org/kb/knowrob.owl#objectActedOn'")
(defvar *goal_location* "'http://knowrob.org/kb/knowrob.owl#goalLocation'")
(defvar *detected_object* "'http://knowrob.org/kb/knowrob.owl#detectedObject'")

;;Class for 1 Shelf
(defclass shelf ()
  ((shelf-name
    :initarg :shelf-name
    :initform (error "Must have name"))
   (shelf-pos-stamped
    :initarg :shelf-pos-stamped
    :initform (error "Must contain pose-stamped"))))

(defvar *fake-pos1*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 0.6675
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-pos2*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 1.665
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-pos3*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 2.6675
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-pos4*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 3.6675
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-shelf1*
  (make-instance 'shelf
                 :shelf-name "shelf_system_0"
                 :shelf-pos-stamped *fake-pos1*))

(defvar *fake-shelf2*
  (make-instance 'shelf
                 :shelf-name "shelf_system_1"
                 :shelf-pos-stamped *fake-pos2*))

(defvar *fake-shelf3*
  (make-instance 'shelf
                 :shelf-name "shelf_system_2"
                 :shelf-pos-stamped *fake-pos3*))

(defvar *fake-shelf4*
  (make-instance 'shelf
                 :shelf-name "shelf_system_3"
                 :shelf-pos-stamped *fake-pos4*))

(defvar *fake-shelf-list*
  (list *fake-shelf1* *fake-shelf2* *fake-shelf3* *fake-shelf4*))

(defvar *fake-floors*
  (list
   (cons 0 (list
            (list 0 0 0.15) (list 0 0.1 0.55) (list 0 0.1 0.88) (list 0 0.1 1.17) (list 0 0.1 1.43)))
   (cons 1 (list
            (list 0 0 0.15) (list 0 0.1 0.38) (list 0 0.1 0.59) (list 0 0.2 1.11) (list 0 0.2 1.42)))
   (cons 2 (list
            (list 0 0 0.15) (list 0 0.1 0.47) (list 0 0.1 0.76) (list 0 0.1 1.06) (list 0 0.1 1.39)))
   (cons 3 (list
            (list 0 0 0.15) (list 0 0.1 0.43) (list 0 0.1 0.68) (list 0 0.1 0.93) (list 0 0.1 1.18) (list 0 0.1 1.43)))))

(defvar *tip-link* "camera_holder_link")
(defvar *root-link* "base_link")
(defvar *giskard-action-name* "/qp_controller/command")

(defvar *joint-names* #("ur5_shoulder_pan_joint"
                       "ur5_shoulder_lift_joint"
                       "ur5_elbow_joint"
                       "ur5_wrist_1_joint"
                       "ur5_wrist_2_joint"
                        "ur5_wrist_3_joint"))

(defvar *start-pos*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header) "map"
                    (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose) -1.5
                    (geometry_msgs-msg:y geometry_msgs-msg:orientation geometry_msgs-msg:pose) -1
                    ))

(defvar *end-pos*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header) "map"
                    (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose) -1.5
                    (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose) 5
                    (geometry_msgs-msg:y geometry_msgs-msg:orientation geometry_msgs-msg:pose) -1
                    ))
