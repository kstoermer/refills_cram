(in-package :refills-cram)

(defvar *navp-controller-actionclient*)

(defvar *transform-listener*)

(defun init-lowlevel ()
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
