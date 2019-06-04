;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: demo.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: April 30, 2019 09:56:31
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2019-04-30 12:08:59 -0500 (Tue, 30 April 2019) $
;;;;  $LastChangedBy: olson $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *kiosk-path* "C:\\Users\\taylor\\Desktop\\CI_TeachingKiosk\\krf\\")
(defvar *kr-files* '("ontology" "academic-fields-small-level4" "courses-2019-2020" "rules" "groups"))

(defun reload-kiosk-mts ()
  (wipe-kiosk-mts)
  (dolist (file *kr-files*)
    (kb:kr-file->kb (concatenate 'string *kiosk-path* file ".krf"))))

(defun wipe-kiosk-mts ()
  (forget-mt 'TeachingKioskMt))

(defun load-kiosk-mts ()
  (dolist (file *kr-files*)
    (kb:kr-file->kb (concatenate 'string *kiosk-path* file ".krf"))))

(defun kiosk-create-user (id email &optional (userType 'NUPerson))
    (fire:kb-store `(isa ,id ,userType) :mt 'TeachingKioskMt)
    (fire:kb-store `(emailOf ,id ,email) :mt 'TeachingKioskMt))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
