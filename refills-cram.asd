(defsystem refills-cram

  :author "Kevin Störmer"
  :maintainer "Kevin Störmer"
  :license "BSD"

  :depends-on (roslisp
               roslisp-utilities
               cl-tf
	       cl-tf2
               actionlib
               geometry_msgs-msg
               sensor_msgs-msg
               giskard_msgs-msg
	       move_base_msgs-msg
	       cram-json-prolog
	       refills_cram_msgs-msg
	       cram-language
	       cram-prolog
               cram-designators
               cram-process-modules
               cram-language-designator-support
               cram-executive)
  :components
  ((:module "src"
    :components
    ((:file "package")
(:file "prolog-facts" :depends-on ("package"))
(:file "giskard-wrapper" :depends-on ("package"))
(:file "low-level" :depends-on ("package"
				"prolog-facts"))
(:file "process-modules" :depends-on ("package" 
				      "prolog-facts"
				      "low-level"))
(:file "high-level" :depends-on ("package" 
				      "prolog-facts"
				      "low-level"
				      "process-modules"))))))
