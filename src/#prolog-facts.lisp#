(in-package :refills-cram)

(defstruct donbot-movement
  "Represents a motion."
  (PoseStamped))

(def-fact-group donbot-movement-designators (motion-grounding)
  
  ;; drive in simple way
  (<- (desig:motion-grounding ?desig (simple-drive ?motion))
    (desig-prop ?desig (:type :driving))
    (desig-prop ?desig (:PoseStamped ?PoseStamped))
    (lisp-fun make-donbot-movement :PoseStamped ?PoseStamped ?motion)))
