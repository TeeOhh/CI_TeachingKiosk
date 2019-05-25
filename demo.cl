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
(defvar *kiosk-path* "C:\\Users\\taylor\\Desktop\\CI_TeachingKiosk\\krf\\")
(defvar *kr-files* '("ontology" "academic-fields-small-level4" "courses" "rules"))

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

;; Demo - 5/16/19
;; (kiosk-create-user 'kio1 'kio1@gmail.com 'NUUndergraduate)
;; (kiosk-create-user 'kio2 'kio2@gmail.com 'NUPhDStudent)
;; (add-info 'kio1 'interests 'ArtificialIntelligence-Topic)
;; (add-info 'kio2 'interests 'ArtificialIntelligence-Topic)
;; (add-info 'kio2 'enrolledInClass 'CompanionCognitiveSystemStudio-Spring2018)
;; (add-info 'kio2 'passedClass 'IntroductiontoArtificialIntelligence-Spring2018)
;; (get-info 'kio1 'recommendCourse)
;; (get-info 'kio2 'recommendCourse)

(defun add-info (id pred var)
    (fire:kb-store `(,pred ,id ,var) :mt 'TeachingKioskMt))

(defun get-info (id pred)
    (fire:query `(,pred ,id ?var) :context 'TeachingKioskMt))

;; ---------------------------- OLD -------------------------------
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
