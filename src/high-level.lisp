(in-package :refills-cram)

(defun main ()
  (roslisp-utilities:startup-ros)
  (roslisp:spin-until (= 0 1) 2))
