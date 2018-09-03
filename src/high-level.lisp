(in-package :refills-cram)

(defvar *registered-shelf-ids* nil)

(defun main ()
  "Main Method later to be called from service"
  (roslisp:start-ros-node "talker")
    (init-lowlevel)
    (init-giskard-wrapper)
    ;;register shelves
    (let ((shelf-ids
            (add-shelves *fake-shelf-list* (add-shelf-system))))
      (setf *registered-shelf-ids* shelf-ids)
      (loop for shelf-id in shelf-ids
            for i from 0 to 10 do
              (add-shelf-floor shelf-id (cdr (nth i *fake-floors*))))
      (print shelf-ids)
    (roslisp:spin-until (= 0 1) 2)))

(defun build-driving-plan (?shelfid ?pose &optional (?shelfpos "middle"))
  (cram-executive:perform
   (if (string= ?shelfid "")
       (an action (type driving) (PoseStamped ?pose))
       (an action (type driving) (loc (a location (type :shelf) (KnowrobID ?shelfid) (Shelfside ?shelfpos)))))))

(defun build-driving-motion (?action)
  (let ((?loc (donbot-action-loc ?action)))
    (cram-executive:perform
     (a motion (type driving) (loc ?loc)))))

(defun build-arm-movement-plan (?pose ?floorid ?armpos)
  (print "lel")
  (cram-executive:perform
   (if (string= ?floorid "")
       (an action (type armMovement) (loc (a location (PoseStamped ?pose))))
       (an action (type armMovement) (loc (a location (type :flooring) (KnowrobID ?floorid) (Armpos ?armpos)))))))

(defun build-arm-movement-motion (?action)
  (print ?action)
  (let ((?loc (donbot-action-loc ?action)))
    (cram-executive:perform
     (a motion (type movingArm) (loc ?loc)))))

(defun build-scanning-plan (?shelfid ?floorid ?armpos)
  (cram-executive:perform
   (if (string= ?floorid "")
       (a action (type scanning) (ShelfID ?shelfid) (PositionOfArm ?armpos))
       (a action (type scanning) (FloorID ?floorid) (PositionOfArm ?armpos)))))

(defun scan-one-shelf-plan (action)
  (print action))
  ;;(build-driving-plan (donbot-action-shelfid action) nil))

(defun scan-multiple-shelfs-plan (action)
  (build-driving-plan (donbot-action-shelfid action) nil))
   

(defun add-shelf-system ()
  "Adds one Shelf-System and returns his id"
  (ros-info "add-shelf-system" "Adding new shelf-system")
  (get-real-string
   (get-result-of-query
    "?R"
    (json-prolog:prolog-simple
     (format nil "belief_new_object(~a, R), rdf_assert(R, knowrob:describedInMap, iaishop:\'IAIShop_0\', belief_state)" *shelf_system*)))))

(defun get-perceived-frame-id (object-id)
  "Gets frameid for object, this id can be used as frame further on"
  (get-real-string
   (get-result-of-query
    "?F"
    (json-prolog:prolog-simple
     (format nil "object_perception_affordance_frame_name(\'~a\', F)" object-id)))))

(defun add-shelves (list-of-shelfes shelf-system-id)
  "Adds shelves into knowrob and world returns list of shelf ids"
  (loop for shelf in list-of-shelfes do
    (let ((result
            (json-prolog:prolog-simple
             (format nil "belief_new_object(~a, ID), rdf_assert(\'~a\', knowrob:properPhysicalParts, ID, belief_state), object_affordance_static_transform(ID, A, [_,_,T,R]), rdfs_individual_of(A, ~a)" *shelf_meter* shelf-system-id *perception_affordance*))))
      (let ((query-pos (get-result-of-query "?T" result))
            (id (get-real-string (get-result-of-query "?ID" result)))
            (shelf-pos (slot-value shelf 'shelf-pos-stamped)))
        (setf *registered-shelf-ids* (nconc *registered-shelf-ids* (list id)))
        (let ((new-pose
                (roslisp:modify-message-copy
                 shelf-pos
                 (geometry_msgs-msg:position geometry_msgs-msg:pose)
                 (geometry_msgs-msg:position (geometry_msgs-msg:pose (substract-list-from-poseStamped shelf-pos query-pos))))))
           (json-prolog:prolog-simple
            (format nil "belief_at_update(\'~a\', ~a)" id (pose-to-prolog new-pose)))
          (ros-info "add-shelfes" "added shelf with id: ~a" id)))))
  (return-from add-shelves *registered-shelf-ids*))

(defun add-shelf-floor (shelf-id floors)
  "adds list of shelf floors into one shelf, will be obsolete later"
  (loop for floor in floors do
    (let ((layer-type
            (if (< (second floor) 0.13)
                (if (< (third floor) 0.2)
                     *shelf_floor_standing_ground*
                     *shelf_floor_standing*)
                *shelf_floor_mounting*)))
      (json-prolog:prolog-simple
       (format nil "belief_shelf_part_at(\'~a\', ~a, ~a, R)" shelf-id layer-type (last floor))))))

(defun get-result-of-query (result-specifier result-list)
  "Gets Result out of query, result-specifier is something like ?A"
  (if result-list
      (loop for x in (car result-list) do
        (if (string= (car x) result-specifier)
            (return-from get-result-of-query (cdr x))))
      (return-from get-result-of-query nil)))

(defun get-real-string (string-query)
  "Extracts String from wierd knowrob return"
  (subseq (string string-query) 2 (- (length (string string-query)) 2)))

(defun pose-to-prolog (pose-stamped)
  "Prolog need special format to percieve a geometry_msgs/PoseStamped"
  (format nil "[\'~a\', _, [~a, ~a, ~a], [~a,~a,~a,~a]]"
          (std_msgs-msg:frame_id (geometry_msgs-msg:header pose-stamped))
          (geometry_msgs-msg:x (geometry_msgs-msg:position (geometry_msgs-msg:pose pose-stamped)))
          (geometry_msgs-msg:y (geometry_msgs-msg:position (geometry_msgs-msg:pose pose-stamped)))
          (geometry_msgs-msg:z (geometry_msgs-msg:position (geometry_msgs-msg:pose pose-stamped)))
          (geometry_msgs-msg:x (geometry_msgs-msg:orientation (geometry_msgs-msg:pose pose-stamped)))
          (geometry_msgs-msg:y (geometry_msgs-msg:orientation (geometry_msgs-msg:pose pose-stamped)))
          (geometry_msgs-msg:z (geometry_msgs-msg:orientation (geometry_msgs-msg:pose pose-stamped)))
          (geometry_msgs-msg:w (geometry_msgs-msg:orientation (geometry_msgs-msg:pose pose-stamped)))))

(defun substract-list-from-poseStamped (PoseStamped list-of-position-xyz)
  "Mathematical operation for substractiong xyz from geometry_msgs/PoseStamped"
  (roslisp:make-msg
   "geometry_msgs/PoseStamped"
   (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose)
   (-
    (geometry_msgs-msg:x (geometry_msgs-msg:position (geometry_msgs-msg:pose PoseStamped))) 
    (coerce (first list-of-position-xyz) 'single-float))
   (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose)
   (-
    (geometry_msgs-msg:y (geometry_msgs-msg:position (geometry_msgs-msg:pose PoseStamped)))
    (coerce (second list-of-position-xyz) 'single-float))
   (geometry_msgs-msg:z geometry_msgs-msg:position geometry_msgs-msg:pose)
   (-
    (geometry_msgs-msg:z (geometry_msgs-msg:position (geometry_msgs-msg:pose PoseStamped)))
    (coerce (third list-of-position-xyz) 'single-float))))
