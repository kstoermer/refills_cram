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

(defun add-shelves (shelf-system-id)
  "Adds shelves into knowrob and world"
  (json-prolog:prolog-simple
   (format nil "belief_new_object(~a, ID), rdf_assert(\'~a\', knowrob:properPhysicalParts, ID, belief_state), object_affordance_static_transform(ID, A, [_,_,T,R]), rdfs_individual_of(A, ~a)" *shelf_meter* shelf-system-id *perception_affordance*)))

(defun get-result-of-query (result-specifier result-list)
  "Gets Result out of query"
  (if result-list
      (loop for x in (car result-list) do
        (if (string= (car x) result-specifier)
            (return-from get-result-of-query (cdr x))))
      (return-from get-result-of-query nil)))

(defun get-real-string (string-query)
  (subseq string-query 3 (- (length string-query) 3)))

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
