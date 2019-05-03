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

(defun kiosk-create-user (id email firstName lastName &optional (userType "NUPerson"))
  (let* ((microtheory (userMicrotheory id))
         (name (concatenate 'string firstName " " lastName))
         (userType (intern userType))
         (id (intern id)))
    (fire:tell-it `(isa ,id Agent-Generic) :context microtheory)
    (fire:tell-it `(nameString ,id ,name) :context microtheory)
    (fire:tell-it `(emailOf ,id ,email) :context microtheory)
    (fire:tell-it `(isa ,id ,userType) :context microtheory)))

(defun request-info (id)
  (let* ((microtheory (userMicrotheory id))
         (id (intern id)))
    (cond ((fire:query `(isa ,id NUStudent) :context microtheory)
           (print "Hi, I see you are an NUStudent.")
           (progn
             (print "What is your Major?")
             (setq major (read-line))
             (fire:tell-it `(studentMajor ,id NUComputerScience) :context microtheory)
             (print "What class are you taking?")
             (setq class (read-line))
             (fire:tell-it `(enrolledInClass ,id ,class) :context microtheory)))
          ((fire:query `(isa ,id NUFaculty) :context microtheory)
           (print "Hi, I see you are an NUFaculty.")
           (progn
             (print "Where is your office?")
             (setq office (read-line))
             (fire:tell-it `(officeLocation ,id ,office) :context microtheory)
             (print "What class are you teaching?")
             (setq class (read-line))
             (fire:tell-it `(teachingClass ,id ,class) :context microtheory))))))

(defun get-info (id pred)
  (let ((microtheory (userMicrotheory id))
        (id (intern id)))
    (fire:query `(,pred ,id ?var) :context microtheory)))

(defun add-info (id pred var)
  (let* ((microtheory (userMicrotheory id))
         (id (intern id)))
    (fire:tell-it `(,pred ,id ,var) :context microtheory)))

(defun userMicrotheory (id)
 (intern (concatenate 'string id "Mt")))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
