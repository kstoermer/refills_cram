(in-package :refills-cram)


;; prebuild variables for prolog queries
(defvar *map* "map")
(defvar *shop* "shop")
(defvar *shelf_floor* (format nil "~a:'ShelfLayer'" *shop*))
(defvar *dm_market* "dmshop")
(defvar *shelf_system* (format nil "~a:'DMShelfSystem'" *dm_market*))
(defvar *shelf_meter* (format nil "~a:'DMShelfFrameFrontStore'" *dm_market*))
(defvar *shelf_floor_standing* (format nil "~a:'DMShelfLayer4TilesFront'" *dm_market*))
(defvar *shelf_floor_standing_ground* (format nil "~a:'DMShelfLayer5TilesFront'" *dm_market*))
(defvar *shelf_floor_mounting* (format nil "~a:'DMShelfLayerMountingFront'" *dm_market*))
(defvar *seperator* (format nil "~a:'DMShelfSeperator4Tiles'" *dm_market*))
(defvar *mounting_bar* (format nil "~a:'DMShelfMountingBar'" *dm_market*))
(defvar *barcode* (format nil "~a:'DMShelfLabel'" *dm_market*))
(defvar *perception_affordance* (format nil "~a:'DMShelfPerceptionAffordance'" *dm_market*))

(defvar *object_acted_on* "'http://knowrob.org/kb/knowrob.owl#objectActedOn'")
(defvar *goal_location* "'http://knowrob.org/kb/knowrob.owl#goalLocation'")
(defvar *detected_object* "'http://knowrob.org/kb/knowrob.owl#detectedObject'")

;;Class for 1 Shelf
(defclass shelf ()
  ((shelf-name
    :initarg :shelf-name
    :initform (error "Must have name"))
   (shelf-pos-stamped
    :initarg :shelf-pos-stamped
    :initform (error "Must contain pose-stamped"))))

(defvar *fake-pos1*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 0.6675
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-pos2*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 1.665
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-pos3*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 2.6675
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-pos4*
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (std_msgs-msg:frame_id std_msgs-msg:header)
                    "map"
                    (geometry_msgs-msg:position geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Point"
                                      (x) 3.6675
                                      (y) -0.59
                                      (z) 0.0 )
                    (geometry_msgs-msg:orientation geometry_msgs-msg:pose)
                    (roslisp:make-msg "geometry_msgs/Quaternion"
                                      (w) 1.0)))

(defvar *fake-shelf1*
  (make-instance 'shelf
                 :shelf-name "shelf_system_0"
                 :shelf-pos-stamped *fake-pos1*))

(defvar *fake-shelf2*
  (make-instance 'shelf
                 :shelf-name "shelf_system_1"
                 :shelf-pos-stamped *fake-pos2*))

(defvar *fake-shelf3*
  (make-instance 'shelf
                 :shelf-name "shelf_system_2"
                 :shelf-pos-stamped *fake-pos3*))

(defvar *fake-shelf4*
  (make-instance 'shelf
                 :shelf-name "shelf_system_3"
                 :shelf-pos-stamped *fake-pos4*))

(defvar *fake-shelf-list*
  (list *fake-shelf1* *fake-shelf2* *fake-shelf3* *fake-shelf4*))

(defvar *fake-floors*
  (list
   (cons 0 (list
            (list 0 0 0.15) (list 0 0.1 0.55) (list 0 0.1 0.88) (list 0 0.1 1.17) (list 0 0.1 1.43)))
   (cons 1 (list
            (list 0 0 0.15) (list 0 0.1 0.38) (list 0 0.1 0.59) (list 0 0.2 1.11) (list 0 0.2 1.42)))
   (cons 2 (list
            (list 0 0 0.15) (list 0 0.1 0.47) (list 0 0.1 0.76) (list 0 0.1 1.06) (list 0 0.1 1.39)))
   (cons 3 (list
            (list 0 0 0.15) (list 0 0.1 0.43) (list 0 0.1 0.68) (list 0 0.1 0.93) (list 0 0.1 1.18) (list 0 0.1 1.43)))))

(defvar *registered-shelf-ids* nil)

(defun main ()
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
  (ros-info "add-shelf-system" "Adding new shelf-system")
  (get-real-string
   (get-result-of-query
    "?R"
    (json-prolog:prolog-simple
     (format nil "belief_new_object(~a, R), rdf_assert(R, knowrob:describedInMap, iaishop:\'IAIShop_0\', belief_state)" *shelf_system*)))))

(defun get-perceived-frame-id (object-id)
  "Gets frameid for object"
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
  "Gets Result out of query"
  (if result-list
      (loop for x in (car result-list) do
        (if (string= (car x) result-specifier)
            (return-from get-result-of-query (cdr x))))
      (return-from get-result-of-query nil)))

(defun get-real-string (string-query)
  (subseq (string string-query) 2 (- (length (string string-query)) 2)))

(defun pose-to-prolog (pose-stamped)
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
