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

(defun main ()
  (roslisp-utilities:startup-ros)
  (roslisp:spin-until (= 0 1) 2))

(defun add-shelf-system () 
  (get-real-string
   (get-result-of-query
    "?R"
    (json-prolog:prolog-simple
     (format nil "belief_new_object(~a, R), rdf_assert(R, knowrob:describedInMap, iaishop:\'IAIShop_0\', belief_state)" *shelf_system*)))))

(defun get-perceived-frame-id (object-id)
  "Gets frameid for object"
  (get-result-of-query
   "?F"
   (json-prolog:prolog-simple
    (format nil "object_perception_affordance_frame_name(\'~a\', F)" object-id))))

(defun add-shelves (list-of-shelfes shelf-system-id)
  "Adds shelves into knowrob and world"
  (loop for shelf in list-of-shelfes do
    (let ((result
            (json-prolog:prolog-simple
             (format nil "belief_new_object(~a, ID), rdf_assert(\'~a\', knowrob:properPhysicalParts, ID, belief_state), object_affordance_static_transform(ID, A, [_,_,T,R]), rdfs_individual_of(A, ~a)" *shelf_meter* shelf-system-id *perception_affordance*))))
      (let ((query-pos (get-result-of-query "?T" result))
            (id (get-real-string (get-result-of-query "?ID" result)))
            (shelf-pos (slot-value shelf 'shelf-pos-stamped)))
        (let ((new-pose
                (roslisp:modify-message-copy
                 shelf-pos
                 (geometry_msgs-msg:position geometry_msgs-msg:pose)
                 (geometry_msgs-msg:position (geometry_msgs-msg:pose (substract-list-from-poseStamped shelf-pos query-pos))))))
          (print
           (json-prolog:prolog-simple
            (format nil "belief_at_update(\'~a\', ~a)" id (pose-to-prolog new-pose)))))))))

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
