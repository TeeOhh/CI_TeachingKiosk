;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: demo
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
;;;(defvar *kiosk-path* "C:\\Users\\taylor\\Desktop\\CI_TeachingKiosk\\")
;;;(defvar *kr-files* '("userModel"))
;;;
;;;(defun reload-dcec-mts ()
;;;  (wipe-kiosk-mts)
;;;  (dolist (file *kr-files*)
;;;    (kb:kr-file->kb (concatenate 'string *kiosk-path* file ".krf"))))
;;;
;;;(defun wipe-kiosk-mts ()
;;;  (forget-mt 'TeachingKioskMt))
;;;
;;;(defun load-kiosk-mts ()
;;;  (dolist (file *kr-files*)
;;;    (kb:kr-file->kb (concatenate 'string *kiosk-path* file ".krf")))

(defun kiosk-create-user (email firstName lastName &optional (userType 'NUPerson))
  ;create unique Microtheory for user
  ;(fire:kb-store '`(SocialModelMtFn ,email)=userMicrotheory)
  (let ((name (concatenate 'string firstName " " lastName)))
    (fire:kb-store `(isa ,email Agent-Generic) :mt 'demoMicrotheory)
    (fire:kb-store `(nameString ,email ,name) :mt 'demoMicrotheory)
    (fire:kb-store `(isa ,email ,userType) :mt 'demoMicrotheory)))

(defun add-info (email &optional (userType nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
