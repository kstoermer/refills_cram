(in-package :refills-cram)

(def-process-module simple-drive-module (motion-designator)
  (roslisp:ros-info (process-modules)
                    "Donbot navigation invoked with motion designator `~a'."
                    motion-designator)
  (destructuring-bind (command donbotMovement) (reference motion-designator)
    (ecase command
      (simple-drive
       (move-base-absolute (donbot-movement-PoseStamped donbotMovement))))))

(defun start (?pose)
  (top-level
    (cram-process-modules:with-process-modules-running (simple-drive-module)
      (let ((p
              (desig:a motion (type driving) (PoseStamped ?pose))))
        (cram-process-modules:pm-execute 'simple-drive-module p)))))
