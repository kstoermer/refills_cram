(in-package :refills-cram)

(defstruct donbot-movement
  "Represents a motion."
  (loc))

(defstruct donbot-action
  "Represents donbot action"
  (loc))
  

(def-fact-group donbot-movement-designators (motion-grounding)
  
  ;; drive to a pose
  (<- (desig:motion-grounding ?desig (drive-to-pos ?motion))
    (desig-prop ?desig (:type :driving))
    (desig-prop ?desig (:loc ?loc))
    (lisp-fun make-donbot-movement :loc ?loc ?motion))
  
  ;; Move Arm to pos  
  (<- (desig:motion-grounding ?desig (move-arm-to-pos ?motion))
    (desig-prop ?desig (:type :movingArm))
    (desig-prop ?desig (:loc ?loc))
    (lisp-fun make-donbot-movement :loc ?loc ?motion))

  ;; Detect Layers movement
  (<- (desig:motion-grounding ?desig (detect-layers ?motion))
    (desig-prop ?desig (:type :detectLayers))
    (lisp-fun make-donbot-movement ?motion)))


(def-fact-group donbot-action-designators (action-grounding)

  ;; simple drive to location action
  (<- (desig:action-grounding ?desig (build-driving-motion ?action))
    (desig-prop ?desig (:type :driving))
    (desig-prop ?desig (:loc ?loc))
    (lisp-fun make-donbot-action :loc ?loc ?action))

  ;; simple move arm to pos action
  (<- (desig:action-grounding ?desig (build-arm-movement-motion ?action))
    (desig-prop ?desig (:type :armMovement))
    (desig-prop ?desig (:loc ?loc))
    (lisp-fun make-donbot-action :loc ?loc ?action))

  ;; detect layers in shelf
  (<- (desig:action-grounding ?desig (resolve-detect-layers-in-shelf-plan ?action))
    (desig-prop ?desig (:type :detectLayersInShelf))
    (desig-prop ?desig (:loc ?loc))
    (lisp-fun make-donbot-action :loc ?loc ?action))

  ;; detect layers woosh action
  (<- (desig:action-grounding ?desig (resolve-detect-layers-here-motion ?action))
    (desig-prop ?desig (:type :detectLayersHere))
    (lisp-fun make-donbot-action ?action))

  ;; drive to shelf and scan flooring
  (<- (desig:action-grounding ?desig (scan-one-floor-plan ?action))
    (desig-prop ?desig (:type :scan-floor))
    (desig-prop ?desig (:loc ?loc))
    (lisp-fun make-donbot-action :loc ?loc ?action))
  
  ;; drive to shelf and scan every floor
  (<- (desig:action-grounding ?desig (scan-multiple-floors-plan ?action))
    (desig-prop ?desig (:type :scanning))
    (desig-prop ?desig (:loc ?loc))
    (lisp-fun make-donbot-action :loc ?loc ?action)))
  
(def-fact-group available-donbot-process-modules (available-process-module matching-process-module)

  (<- (available-process-module motion-module))

  (<- (matching-process-module ?desig motion-module)
    (desig-prop ?desig (:type :driving)))
  (<- (matching-process-module ?desig motion-module)
    (desig-prop ?desig (:type :movingArm)))
  (<- (matching-process-module ?desig motion-module)
    (desig-prop ?desig (:type :detectLayers))))

(defun get-location (designator)
  (ros-info (location-designator) "Location-designator resolved")
  (with-desig-props (type PoseStamped KnowrobID Shelfside) designator
    (if (eql PoseStamped nil)
        (ecase type
          (:shelf
           (publish-marker (geometry_msgs-msg:pose (get-shelf-pose KnowrobID Shelfside)) (random 1000) "/map")
           (list (get-shelf-pose KnowrobID Shelfside)))
          (:flooring-board
           (publish-marker (geometry_msgs-msg:pose (look-at-floor-position KnowrobID)) (random 1000) "/base_footprint")
           (list (look-at-floor-position KnowrobID)))
          (:flooring-contents
           (publish-marker (geometry_msgs-msg:pose (look-into-floor-position KnowrobID)) (random 1000) "/base_footprint")
           (list (look-into-floor-position KnowrobID))))
        (return-from get-location (list PoseStamped)))))

(register-location-generator 5 get-location)



