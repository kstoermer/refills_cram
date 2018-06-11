(in-package :refills-cram)

(defvar *navp-controller-actionclient*)

(defvar *transform-listener*)

(defun init-lowlevel ()
  (actionlib:start-action-server "testAction" "refills_cram_msgs/testActionAction"
                                 #'addition :separate-thread t)
  (setf *navp-controller-actionclient*
        (actionlib:make-action-client "/nav_pcontroller/move_base" "move_base_msgs/MoveBaseAction"))
  (setf *transform-listener*
        (make-instance 'cl-tf:transformer)))

(roslisp-utilities:register-ros-init-function init-lowlevel)

(defun transform-into-frame (frame pose)
  (cl-tf:to-msg
   (cl-tf:transform-pose-stamped *transform-listener*
                                 :pose (cl-tf:from-msg pose)
                                 :target-frame frame)))

(defun move-base-absolute (target-pose)
  (actionlib:call-goal *navp-controller-actionclient*
                       (actionlib:make-action-goal *navp-controller-actionclient*
                         :target_pose
                         (transform-into-frame "/map" target-pose))))

(actionlib:def-exec-callback addition (testValue)
  (roslisp:ros-info (callback) "callback called")
  (actionlib:succeed-current :testResult (+ 1 testValue)))

(defun transformation-Pose-Stamped (pose &optional (endFrame "/base_footprint")) 
  "transform a Pose-Stamped-msgs with an optional Frame, default is base_footprint" 
  (roslisp:with-fields 
      ((startFrame 
        (STD_msgs-msg:frame_id geometry_msgs-msg:header)) 
       (x 
        (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose)) 
       (y 
        (geometry_msgs-msg:y geometry_msgs-msg:position  geometry_msgs-msg:pose)) 
       (z 
        (geometry_msgs-msg:z geometry_msgs-msg:position geometry_msgs-msg:pose))
       (w
        (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose))
       (xo
        (geometry_msgs-msg:x geometry_msgs-msg:orientation geometry_msgs-msg:pose))
       (yo
        (geometry_msgs-msg:x geometry_msgs-msg:orientation geometry_msgs-msg:pose))
       (zo
        (geometry_msgs-msg:z geometry_msgs-msg:orientation geometry_msgs-msg:pose))) 
      pose 
    (let 
        ((transform-listener 
           (make-instance 'cl-tf:transform-listener)) 
         (tf-pose-stamped 
           (cl-tf:make-pose-stamped startFrame 0.0 
                                    (cl-transforms:make-3d-vector x y z)
                                    (cl-transforms:make-quaternion xo yo zo w)))) 
      (catch-Transformation-Pose-Stamped transform-listener tf-pose-stamped endFrame))))

(defun catch-Transformation-Pose-Stamped (transform-listener tf-pose-stamped endFrame)
  "transform-listener catch transformation (helpfunction)"
  (sleep 5.0)
  (cl-tf:transform-pose-stamped transform-listener
                                :pose tf-pose-stamped
:target-frame endFrame))
