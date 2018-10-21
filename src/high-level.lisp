(in-package :refills-cram)

(defvar *registered-shelf-ids* nil)
(defvar *directive* (list ':front ':end))

(defvar *barcode_adder* (format nil "http://knowrob.org/kb/shop.owl#ProductWithAN"))
(setf *seperator* (format nil "~a:'DMShelfSeparator4Tiles'" *dm_market*))
(defvar *barcode* (format nil "~a:'DMShelfLabel'" *dm_market*))

(defvar *list-of-barcodes* (list
                            (cons 469671 0.1)
                            (cons 252312 0.3)
                            (cons 251219 0.5)
                            (cons 251252 0.7)
                            (cons 309649 0.9)))

(defvar *list-of-seperators* (list
                              0
                              0.2
                              0.4
                              0.6
                              0.8
                              1))

(defun main ()
  "Main Method later to be called from service"
  (roslisp:start-ros-node "talker")
    (init-lowlevel)
    (init-giskard-wrapper)
    ;;register shelves
    (let ((shelf-ids
            (add-shelves *fake-shelf-list* (add-shelf-system))))
      (setf *registered-shelf-ids* shelf-ids)
      (print shelf-ids)
    (roslisp:spin-until (= 0 1) 2)))

(defun rev (list)
  (do ((list list (rest list))
       (reversed '() (list* (first list) reversed)))
      ((endp list) reversed)))

(defun insertFakeFlooringsForShelf (shelfid)
  (loop for id in *registered-shelf-ids*
        for i from 1 to 10 do
          (if (string= id shelfid)
              (add-shelf-floor shelfid (cdr (nth (- i 1) *fake-floors*))))))

(defun build-driving-plan (?shelfid ?pose &optional (?shelfpos :middle))
  (cram-executive:perform
   (if (string= ?shelfid "")
       (an action (type driving) (loc (a location (PoseStamped ?pose))))
       (an action (type driving) (loc (a location (type :shelf) (KnowrobID ?shelfid) (Shelfside ?shelfpos)))))))

(defun build-driving-motion (?action)
  (ros-info (resolve-action-designator) "Actiondesignator-resolved")
  (let ((?loc (donbot-action-loc ?action)))
    (cram-executive:perform
     (a motion (type driving) (loc ?loc)))))

(defun build-detect-layers-in-shelf-plan (?shelfid)
  (cram-executive:perform
   (an action (type detectLayersInShelf) (loc (a location (type :shelf) (KnowrobID ?shelfid) (Shelfside :middle)))))
  (insertfakeflooringsforshelf ?shelfid))

(defun resolve-detect-layers-in-shelf-plan (?action)
  (ros-info (resolve-action-designator) "Actiondesignator-resolved")
  (let ((?loc (donbot-action-loc ?action)))
    (let ((Shelfid (desig:desig-prop-value ?loc :KnowrobID)))
      (cram-executive:perform
       (an action (type driving) (loc ?loc)))
      (cram-executive:perform
       (an action (type detectLayersHere)))
      (insertfakeflooringsforshelf Shelfid))))

(defun resolve-detect-layers-here-motion (?action)
  (ros-info (resolve-action-designator) "Actiondesignator-resolved")
  (cram-executive:perform
   (a motion (type detectLayers))))

(defun build-arm-movement-motion (?action)
  (ros-info (resolve-action-designator) "Actiondesignator-resolved")
  (let ((?loc (donbot-action-loc ?action)))
    (cram-executive:perform
     (a motion (type movingArm) (loc ?loc)))))

(defun scan-one-floor-plan (?action)
  (ros-info (resolve-action-designator) "Actiondesignator-resolved")  
  (let ((?loc (donbot-action-loc ?action)))
    (let ((?Floor-id (desig:desig-prop-value ?loc :KnowrobID)))
      (let ((?Shelf-id (get-shelf-for-floor ?Floor-id)))
        (let ((?first-directive (first *directive*)))
          (let ((?second-directive (second *directive*)))
            (cram-executive:perform
             (an action (type driving) (loc
                                        (a location
                                           (type shelf)
                                           (KnowrobID ?Shelf-id)
                                           (Shelfside ?first-directive)))))
            (cram-executive:perform
             (an action (type armMovement) (loc ?loc)))
            (cram-executive:perform
             (an action (type driving) (loc
                                        (a location
                                           (type shelf)
                                           (KnowrobID ?Shelf-id)
                                           (Shelfside ?second-directive))))))))
      (setf *directive* (rev *directive*)))))

(defun scan-multiple-floors-plan (?action)
  (ros-info (resolve-action-designator) "Actiondesignator-resolved")
  (let ((?loc (donbot-action-loc ?action)))
    (let ((?Shelf-id (desig:desig-prop-value ?loc :KnowrobID)))
      (cram-executive:perform
       (an action (type detectLayersInShelf) (loc
                                              (a location
                                                 (type shelf)
                                                 (KnowrobID ?Shelf-id)
                                                 (Shelfside :middle)))))
      (let ((floorlist (get-floors-for-shelf ?Shelf-id)))
        (loop for ?floorid in floorlist do
          (cram-executive:perform
           (an action (type scan-floor) (loc
                                         (a location
                                            (type flooring-board)
                                            (KnowrobID ?floorid))))))
        (loop for ?floorid in floorlist do
          (cram-executive:perform
           (an action (type scan-floor) (loc
                                         (a location
                                            (type flooring-contents)
                                            (KnowrobID ?floorid))))))))))

(defun add-shelf-system ()
  "Adds one Shelf-System and returns his id"
  (ros-info "add-shelf-system" "Adding new shelf-system")
  (get-real-string
   (get-result-of-query
    "?R"
    (json-prolog:prolog-simple
     (format nil "belief_new_object(~a, R), rdf_assert(R, knowrob:describedInMap, iaishop:\'IAIShop_0\', belief_state)" *shelf_system*)))))

(defun add-facings-to-floor (floor-id)
  "adds 5 facings to one floor"
  (loop for x from 0 to 5 do
    (add-seperator-for-shelf floor-id (nth x *list-of-seperators*)))
  (loop for y from 0 to 4 do
    (add-barcode-for-shelf floor-id (car (nth y *list-of-barcodes*)) (cdr (nth y *list-of-barcodes*)))))

(defun add-seperator-for-shelf (floor-id x)
  (json-prolog:prolog-simple
   (format nil "belief_shelf_part_at(\'~a\', ~a, ~a, R)" floor-id *seperator* x)))

(defun add-barcode-for-shelf (floor-id barcode x)
  (json-prolog:prolog-simple
   (format nil
           "belief_shelf_barcode_at(\'~a\', ~a, dan(\'~a\'), ~a, R)"
           floor-id
           *barcode*
           barcode
           x)))

(defun get-facing-for-barcode (barcode)
  (get-real-string
   (get-result-of-query
    "?F"
    (json-prolog:prolog-simple
     (let ((barcode-string
             (concatenate 'string *barcode_adder* barcode)))
       (format nil
               "shelf_facing(_, F), shelf_facing_product_type(F,\'~a\')" barcode-string))))))

(defun get-floor-for-barcode (barcode)
  (get-real-string
   (get-result-of-query
    "?F"
    (json-prolog:prolog-simple
     (let ((barcode-string
             (concatenate 'string *barcode_adder* barcode)))
       (format nil
               "shelf_facing_product_type(E,\'~a\'), shelf_facing(F, E)" barcode-string))))))

(defun get-perceived-frame-id (object-id)
  "Gets frameid for object, this id can be used as frame further on"
  (get-real-string
   (get-result-of-query
    "?F"
    (json-prolog:prolog-simple
     (format nil "object_perception_affordance_frame_name(\'~a\', F)" object-id)))))

(defun get-object-frame-id (object-id)
  (get-real-string
   (get-result-of-query
    "?R"
    (json-prolog:prolog-simple
     (format nil "object_frame_name(\'~a\', R)." object-id)))))

(defun get-shelf-for-floor (floor-id)
  "Gets floorids for every floor in a shelf"
  (get-real-string
   (get-result-of-query
    "?Meter"
    (json-prolog:prolog-simple
     (format nil "owl_individual_of(\'~a\', 'http://knowrob.org/kb/dm-market.owl#DMShelfLayer'), rdf_has(Meter, knowrob:properPhysicalParts, \'~a\'), owl_individual_of(Meter, 'http://knowrob.org/kb/dm-market.owl#DMShelfFrame')." floor-id floor-id)))))

(defun get-floors-for-shelf (shelf-id)
  "Gets floorids for every floor in a shelf"
  (get-real-string-list
   (get-result-of-query
    "?Layers"
    (json-prolog:prolog-simple
     (format nil "owl_individual_of(\'~a\', 'http://knowrob.org/kb/dm-market.owl#DMShelfFrame'), findall(_Layer, (rdf_has(\'~a\', knowrob:properPhysicalParts, _Layer), owl_individual_of(_Layer, 'http://knowrob.org/kb/dm-market.owl#DMShelfLayer')), Layers)." shelf-id shelf-id)))))


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

(defun get-real-string-list (list)
  "Extracts String in list from wierd knowrob return"
  (map 'list
       (lambda (x) (subseq (string x) 1 (- (length (string x)) 1)))
       list))

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
