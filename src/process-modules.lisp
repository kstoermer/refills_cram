(in-package :refills-cram)

(def-process-module motion-module (motion-designator)
  (roslisp:ros-info (process-modules)
                    "Donbot navigation invoked with motion designator `~a'."
                    motion-designator)
  (destructuring-bind (command donbotMovement) (reference motion-designator)
    (ecase command
      (drive-to-pos
       (move-base-absolute (reference (donbot-movement-loc donbotMovement))))
      (move-arm-to-pos
       (move-arm-to-pose (reference (donbot-movement-loc donbotMovement))))
      (detect-layers
       (floor-detection-pose2)
       (floor-detection-pose)))))

(defun start (&optional ?pose ?shelfid)
  (top-level
    (cram-process-modules:with-process-modules-running (motion-module)
      (let ((p
              (desig:a motion (type loc) (PoseStamped "uh"))))
        (reference p)))))
