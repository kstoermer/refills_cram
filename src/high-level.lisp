(in-package :refills-cram)


;; prebuild variables for prolog queries
(defvar *map* "map")
(defvar *shop* "shop")
(defvar *shelf_floor* (format t "~a:'ShelfLayer'" *shop*))
(defvar *dm_market* "dmshop")
(defvar *shelf_system* (format t "~a:'DMShelfSystem'" *dm_market*))
(defvar *shelf_meter* (format t "~a:'DMShelfFrameFrontStore'" *dm_market*))
(defvar *shelf_floor_standing* (format t "~a:'DMShelfLayer4TilesFront'" *dm_market*))
(defvar *shelf_floor_standing_ground* (format t "~a:'DMShelfLayer5TilesFront'" *dm_market*))
(defvar *shelf_floor_mounting* (format t "~a:'DMShelfLayerMountingFront'" *dm_market*))
(defvar *seperator* (format t "~a:'DMShelfSeperator4Tiles'" *dm_market*))
(defvar *mounting_bar* (format t "~a:'DMShelfMountingBar'" *dm_market*))
(defvar *barcode* (format t "~a:'DMShelfLabel'" *dm_market*))
(defvar *perception_affordance* (format t "~a:'DMShelfPerceptionAffordance'" *dm_market*))

(defvar *object_acted_on* "'http://knowrob.org/kb/knowrob.owl#objectActedOn'")
(defvar *goal_location* "'http://knowrob.org/kb/knowrob.owl#goalLocation'")
(defvar *detected_object* "'http://knowrob.org/kb/knowrob.owl#detectedObject'")

(defun main ()
  (roslisp-utilities:startup-ros)
  (roslisp:spin-until (= 0 1) 2))

(defun add_shelf_system ()
  (json-prolog:prolog-simple
   (format t "belief_new_object(~a, R), rdf_assert(R, knowrob:describedInMap, iaishop:\'IAIShop_0\', belief_state)" *shelf_system*))) 
