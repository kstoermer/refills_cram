(in-package :refills-cram)

(defvar *actionclient-giskard*)

(defun init-giskard-wrapper ()
  "initialize giskard wrapper"
  (setf *actionclient-giskard*
        (actionlib:make-action-client
         *giskard-action-name*
         "giskard_msgs/ControllerListAction")))

(roslisp-utilities:register-ros-init-function init-giskard-wrapper)

(defun build-joint-goal (joint-state)
  "Build joint goal out of joint-state message"
  (actionlib:make-action-goal
      *actionclient-giskard*
    (giskard_msgs-msg:type) 0
    (giskard_msgs-msg:controllers)
    (vector
     (roslisp:make-msg
     "giskard_msgs/Controller"
     (giskard_msgs-msg:type) 1
     (giskard_msgs-msg:max_speed) 2
     (giskard_msgs-msg:tip_link) *tip-link*
     (giskard_msgs-msg:root_link) *root-link*
     (giskard_msgs-msg:goal_state) joint-state
     (giskard_msgs-msg:p_gain) 3
     (giskard_msgs-msg:weight) 1.0))))

(defun move-arm-drivepos ()
  "Drive pos for robot. Experimental"
  (actionlib:send-goal-and-wait *actionclient-giskard* 
                       (build-joint-goal
                        (roslisp:make-msg
                         "sensor_msgs/JointState"
                         (sensor_msgs-msg:name) *joint-names*
                         (sensor_msgs-msg:position)
                         #(-1.54838782946
                           -2.51830751101
                           1.37984895706
                           -2.07125074068
                           -1.59281522432
                           1.56871032715)))))
  
(defun floor-detection-pose ()
  "floor detection pos for robot. Experimental"
  (actionlib:send-goal-and-wait *actionclient-giskard* 
                       (build-joint-goal
                        (roslisp:make-msg
                         "sensor_msgs/JointState"
                         (sensor_msgs-msg:name) *joint-names*
                         (sensor_msgs-msg:position)
                         #(-1.75002080599
                           -2.07400399843
                           1.79469525513
                           -2.49168950716
                           -1.52502566973
                           1.5591506958)))))

(defun floor-detection-pose2 ()
  "floor detection pos for robot. Experimental"
  (actionlib:send-goal-and-wait *actionclient-giskard* 
                       (build-joint-goal
                        (roslisp:make-msg
                         "sensor_msgs/JointState"
                         (sensor_msgs-msg:name) *joint-names*
                         (sensor_msgs-msg:position)
                         #(-1.61822063128
                           -1.36299354235
                           1.37984895706
                           -2.07125074068
                           -1.59281522432
                           1.56871032715)))))
(defun baseboard-pose ()
  "baseboard pos for robot. Experimental"
  (actionlib:send-goal-and-wait *actionclient-giskard* 
                       (build-joint-goal
                        (roslisp:make-msg
                         "sensor_msgs/JointState"
                         (sensor_msgs-msg:name) *joint-names*
                         (sensor_msgs-msg:position)
                         #(-1.56896144549
                           -1.2928908507
                           1.59626483917
                           -2.61700326601
                           -1.54054629803
                           1.56871032715)))))
