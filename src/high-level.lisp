(in-package :refills-cram)

(defvar *registered-shelf-ids* nil)

(defun main ()
  "Main Method later to be called from service"
  (roslisp:with-ros-node ("talker")
    (init-lowlevel)
    (init-giskard-wrapper)
    (wait-duration 1)
    ;; (roslisp-utilities:startup-ros)
    ;; First register shelf-system and shelfes
    (let ((shelf-ids
            (add-shelves *fake-shelf-list* (add-shelf-system))))
      (move-arm-drivepos)
      (loop for shelf-id in shelf-ids do
        (mark-shelf shelf-id)
        (ros-info "main" "traversing to front of shelf ~a" shelf-id)
        (traverse-to-shelf shelf-id "front")
        (wait-duration 5)
        (ros-info "main" "traversing to end of shelf ~a" shelf-id)
        (traverse-to-shelf shelf-id "end")
        (wait-duration 10)))
    (roslisp:spin-until (= 0 1) 2)))

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
