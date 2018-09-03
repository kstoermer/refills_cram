(in-package :refills-cram)

(defvar *navp-controller-actionclient*)

(defvar *buffer-client*)

(defvar *marker-publisher*)

(defvar *current-pos*
  (roslisp:make-message "geometry_msgs/PoseStamped"
                        (std_msgs-msg:frame_id geometry_msgs-msg:header)
                        "/base_footprint"
                        (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                        1))

(defvar *right-orientation*
  (roslisp:make-msg "geometry_msgs/Quaternion"
                    (geometry_msgs-msg:y) -1))

(defvar *front-of-shelf*
  (roslisp:make-msg "geometry_msgs/Point"
                    (x)
                    0
                    (y)
                    -1.3
                    (z)
                    0))

(defvar *mid-of-shelf*
  (roslisp:make-msg "geometry_msgs/Point"
                    (x)
                    0.5
                    (y)
                    -1.3
                    (z)
                    0))

(defvar *end-of-shelf*
  (roslisp:make-msg "geometry_msgs/Point"
                    (x)
                    1
                    (y)
                    -1.3
                    (z)
                    0))
                    

(defun init-lowlevel ()
  "Initialization for this file"
  (setf *marker-publisher*
        (roslisp:advertise "/visualization_marker" "visualization_msgs/Marker"))
  (actionlib:start-action-server "donbot" "refills_cram_msgs/donbotAction"
                                 #'addition :separate-thread t)
  (setf *navp-controller-actionclient*
        (actionlib:make-action-client "/nav_pcontroller/move_base" "move_base_msgs/MoveBaseAction"))
  (setf *buffer-client*
        (make-instance 'cl-tf2:buffer-client)))

(roslisp-utilities:register-ros-init-function init-lowlevel)

(defun transform-posestamped-into-frame (frame pose)
  "Simple transformation method, transforms PoseStaped into frame"
  (cl-tf2:to-msg
   (cl-tf2:transform-pose-stamped *buffer-client*
                                 :pose (cl-tf:from-msg pose)
                                 :target-frame frame)))

(defun get-shelf-pose (shelf-id &optional (side "middle"))
  (transform-posestamped-into-frame
   "/map"
   (roslisp:make-msg "geometry_msgs/PoseStamped"
                     (std_msgs-msg:frame_id std_msgs-msg:header)
                     (get-perceived-frame-id shelf-id)
                     (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                     1
                     (geometry_msgs-msg:position geometry_msgs-msg:pose)
                     (if (string= side "front") *front-of-shelf*
                         (if (string= side "middle") *mid-of-shelf* *end-of-shelf*)))))
  

(defun move-base-absolute (target-pose)
  "Move base to target PoseStamped"
  (actionlib:send-goal-and-wait *navp-controller-actionclient*
                       (actionlib:make-action-goal *navp-controller-actionclient*
                         :target_pose
                         (transform-posestamped-into-frame "/map" target-pose))))

(defun traverse-to-shelf (shelf-id &optional (side "middle"))
  "Robot drives to front or end of shelf. Contains safetymeasures to make sure robot doesnt bump into things"
  (roslisp:ros-info "traverse-to-shelf" "calculation traversion to ~a" shelf-id)
  (let ((cur-pos
          (transform-posestamped-into-frame
           "/map"
           *current-pos*))
        (shelf-pos
          (transform-posestamped-into-frame
           "/map"
           (roslisp:make-msg "geometry_msgs/PoseStamped"
                             (std_msgs-msg:frame_id std_msgs-msg:header)
                             (get-perceived-frame-id shelf-id)
                             (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                             1
                             (geometry_msgs-msg:position geometry_msgs-msg:pose)
                             (if (string= side "front") *front-of-shelf*
                                 (if (string= side "middle") *mid-of-shelf* *end-of-shelf*))))))
    (if (is-in-position-already shelf-pos)
        (ros-info "traverse-to-shelf" "Already in position, not moving")
        (progn
          (move-base-absolute
           (roslisp:modify-message-copy
            cur-pos
            (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose)
            (geometry_msgs-msg:y
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose shelf-pos)))
            (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
            *right-orientation*))
          (ros-info "traverse-to-shelf" "traverse to shelf now")
          (move-base-absolute
           (roslisp:modify-message-copy
            shelf-pos
            (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
            *right-orientation*))))))

(actionlib:def-exec-callback addition (type shelfid floorid pose)
  (roslisp:ros-info (callback) "callback called")
  
  ;(actionlib:succeed-current :testResult (+ 1 testValue)))
  )

(defun decide-plan (type shelfid floorid pose)
  (top-level
    (cram-process-modules:with-process-modules-running (motion-module)
      (cond
        ((eql type 0)
         (build-driving-plan shelfid pose))
        ((eql type 1)
         (build-arm-movement-plan pose floorid :upper))
        ((eql type 2)
         (build-arm-movement-plan pose floorid :lower))
        ((eql type 3)
         (build-scanning-plan shelfid floorid :upper))
        ((eql type 4)
         (build-scanning-plan shelfid floorid :lower))))))

(defun mark-shelf (shelf-id)
  "Visualize shelf front end end markings. For debugging purposes"
  (publish-marker
   (roslisp:make-msg "geometry_msgs/Pose"
                     (geometry_msgs-msg:w geometry_msgs-msg:orientation)
                     1
                     (geometry_msgs-msg:position)
                     *front-of-shelf*
                     )
   (random 100000)
   (get-perceived-frame-id shelf-id))
  (publish-marker
   (roslisp:make-msg "geometry_msgs/Pose"
                     (geometry_msgs-msg:w geometry_msgs-msg:orientation)
                     1
                     (geometry_msgs-msg:position)
                     *end-of-shelf*
                     )
   (random 100000)
   (get-perceived-frame-id shelf-id))
   )

(defun publish-marker (pose id frame)
  "Pubish yellow sphere marker"
  (roslisp:publish *marker-publisher*
                     (roslisp:make-message "visualization_msgs/Marker" (frame_id header) frame
                                         ns "planning_namespace"
                                         id id
                                         type 2
                                         action 0
                                         pose pose
                                         (x scale) 0.1
                                         (y scale) 0.1
                                         (z scale) 0.1
                                         (r color) 1.0
                                         (g color) 0.8
                                         (b color) 0.0
                                         (a color) 1.0)))

(defun compare-pose-stamped (pose1 pose2 deltax deltay deltaz)
  "Compares xyz of 2 PoseStampeds to a delta"
  (let
      ((dx
         (abs
          (-
           (geometry_msgs-msg:x (geometry_msgs-msg:position (geometry_msgs-msg:pose pose1)))
           (geometry_msgs-msg:x (geometry_msgs-msg:position (geometry_msgs-msg:pose pose2))))))
       (dy
         (abs
          (-
           (geometry_msgs-msg:y (geometry_msgs-msg:position (geometry_msgs-msg:pose pose1)))
           (geometry_msgs-msg:y (geometry_msgs-msg:position (geometry_msgs-msg:pose pose2))))))
       (dz
         (abs
          (-
           (geometry_msgs-msg:z (geometry_msgs-msg:position (geometry_msgs-msg:pose pose1)))
           (geometry_msgs-msg:z (geometry_msgs-msg:position (geometry_msgs-msg:pose pose2)))))))
    (if (and (<= dx deltax) (<= dy deltay) (<= dz deltaz))
        (return-from compare-pose-stamped t)
        (return-from compare-pose-stamped nil))))

(defun is-in-position-already (pose)
  "Checks if robot is in given position already (with some delta)"
  (let
      ((current-pos
         (transform-posestamped-into-frame "/map" *current-pos*))
       (pose-to-drive-to
         (transform-posestamped-into-frame "/map" pose)))
    (compare-pose-stamped current-pos pose-to-drive-to 0.1 0.1 1000)))    
