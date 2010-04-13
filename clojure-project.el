;; clojure-project.el --- project manager for swank-clojure
;;
;; Copyright (C) 2010 Pavel Polechtchouk
;;
;; Author: Pavel Polechtchouk <pavel.overseas@hotmail.com>
;;
;; URL: http://github.com/ppolechtchouk/clojure-project
;; Version: 0.9.0
;; Keywords: languages, lisp
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).
;;
;;; Commentary:
;; Version history:
;; 0.9.0 Adapted to swank-clojure 1.6

(require 'slime)
(require 'swank-clojure)


(defvar clojure-projects-root-dir "~/projects"
"Root directory that contains your Clojure projects")

(defvar clojure-project nil
"Name of the currently open project")

(defvar clojure-project-open-after-creating-package-p t
"If not nil open the clojure file once you create it with 'clojure-project-creat-package'")

(defvar clojure-project-use-swank-clojure-jars-p t
"If set to t , when 'clojure-project-open' is called, it will automatically load all the swank-clojure jars.
Otherwise, the swank-clojure and clojure jars must be in the lib directory.")

(defvar clojure-project-set-compil-dir-p nil
"If t , sets the clojure.compile.path parameter to classes directory")

(defvar clojure-project-buffer-name "*Clojure Projects*")

(defun clojure-project-use-swank-jars ()
; project-dir would be set when this is called
    (setq swank-clojure-classpath 
        (append swank-clojure-classpath
            (swank-clojure-default-classpath))))
 
;; TODO. needs checking if classes will compile automatically 
(defun clojure-project-set-compil-path ()
; project-dir would be set when this is called
    (if (boundp 'swank-clojure-extra-vm-args)
            (add-to-list 'swank-clojure-extra-vm-args
                     (format "-Dclojure.compile.path=%s"
                             (expand-file-name "classes/" project-dir)))
            (setq swank-clojure-extra-vm-args
                (list  (format "-Dclojure.compile.path=%s"
                           (expand-file-name "classes/" project-dir))))))

(defun clojure-project-open ()
  (interactive)
  (let ((project)
        (project-dir)
       )         
    (clojure-project-show-projects) ;show a list of existing projects  
    (setq project
    (completing-read "Project name? " (clojure-project-list)))
    (setq project-dir (clojure-project-dir project))  
    (if (not (clojure-project-structure-p project-dir))
      (error "Not a valid clojure project"))

    (setq clojure-project project); set currently opened project
    (clojure-project-show-projects);update project list
    (message (format "Starting clojure project %s" project))
    
; Set up all hooks
    (if clojure-project-use-swank-clojure-jars-p
        (add-hook 'swank-clojure-project-hook 'clojure-project-use-swank-jars))

; Start slime
    (swank-clojure-project project-dir)))

(defun clojure-project-structure-p (root-dir)
  "Returns t if directory structure under root-dir conforms to a clojure project layout"
 (and
  (file-directory-p root-dir)
  (file-directory-p (concat root-dir "/src"))
  (file-directory-p (concat root-dir "/classes"))
  (file-directory-p (concat root-dir "/test"))
  (file-directory-p (concat root-dir "/lib"))))

(defun clojure-project-create (&optional project)
  "Creates a clojure project layout in the specified directory."
  (interactive)
  (unless project
    (clojure-project-show-projects) ;show a list of existing projects
    (let ((project)
	  (dir))
      (setq project (read-string
       "Creating a new Clojure project\nEnter project name: "))
      (clojure-project-create project)))
  (let ((dir))
    (setq dir (clojure-project-dir project))
    (if (file-directory-p dir)
	(error
	 "Project '%s' could not be created. Directory '%s' already exists" 
	 (propertize project 'face 'bold)
	 (propertize (expand-file-name dir) 'face 'bold)))
    (make-directory dir)
    (make-directory (concat dir "/src"))
    (make-directory (concat dir "/classes"))
    (make-directory (concat dir "/test"))
    (make-directory (concat dir "/lib"))
    (clojure-project-show-projects) ;show an updated list of existing projects
  ))

(defun clojure-project-list ()
  "returns a list of the clojure projects in the directory defined by
 'clojure-projects-root-dir'
A project is something that conforms to the directory structure"
  (let ((dirs (directory-files  clojure-projects-root-dir))
	      (result))
    (dolist (elt dirs result)
      (if (clojure-project-structure-p (clojure-project-dir elt))
	  (setq result (cons elt result))))))

(defun clojure-project-show-projects (&optional dir)
"Shows a list of existing Clojure projects in the directory defined by
 'clojure-projects-root-dir'"
  (interactive)
  (let ( (buf (get-buffer-create  clojure-project-buffer-name))
	 (inhibit-read-only t))
    (save-current-buffer
      (set-buffer buf)
      (erase-buffer)
      (insert (format "Clojure projects in the %s directory\n"
		 (propertize
		  (expand-file-name clojure-projects-root-dir)
			     'face 'bold)))
      (insert 
       (mapconcat 'clojure-project-highlight (clojure-project-list) " | "))
      (setq inhibit-read-only nil)      
      (view-buffer buf))))

(defun clojure-project-highlight (project)
  "Sets the name of the currently open project to bold"
  (if (equal clojure-project project)
      (propertize project 'face 'bold)
    project))

(defun clojure-project-dir (project)
  (concat clojure-projects-root-dir "/" project))

(defun clojure-project-valid-dir-p (dir-name)
"Returns t if directory name conforms to the naming conventions -
no '.', spaces etc. Note that 'dir-name' is not a path"
(let* ((illegal-str (list "." " " "/") )
      (regx (regexp-opt illegal-str)))
  (if (string-match-p regx dir-name) nil t)))


(defun clojure-project-packages (project)
  "Returns a list of clojure packages in the 'src' directory of 'project'" 
  (clojure-project-packages1 
   (concat (clojure-project-dir project) "/src")))

(defun clojure-project-packages1 (dir &optional parents)
;; recursive function that does the work
 (let ( (clj-packages-list)
	(current-file)
	(current-file-name)
	(current-file-list (directory-files-and-attributes dir nil "^[^\\.]") ))
   (while current-file-list
     (setq current-file (car current-file-list))
     (setq current-file-name (car current-file))
     (cond 
      ((eq t (car (cdr current-file)))	; is it a directory?
       (if (clojure-project-valid-dir-p  current-file-name)
	   (setq clj-packages-list 
		 (append (clojure-project-packages1 
		   (concat dir "/" current-file-name)
		   (concat parents "." current-file-name)) clj-packages-list))
	; do nothing if dir is not valid
	 ))				
      ((and
	(equal ".clj" (substring current-file-name -4))
	(clojure-project-valid-dir-p (substring current-file-name 0 -4)))
       (setq clj-packages-list
	(cons 
	 (substring 			; remove the first '.'
	  (concat parents "." (substring current-file-name 0 -4) ) 1)
	 clj-packages-list))))
     (setq current-file-list (cdr current-file-list)))
   ;return the package list
   clj-packages-list))

(defun clojure-project-show-packages ()
"Shows a list of existing packages for the currently open project"
  (interactive)
  (if (not clojure-project)
      (error "no Clojure project open"))
  (let ( (buf (get-buffer-create  clojure-project-buffer-name))
	 (inhibit-read-only t))
    (save-current-buffer
      (set-buffer buf)
      (erase-buffer)
      (insert (format "Clojure project %s packages\n"
		 (propertize
		  (expand-file-name (clojure-project-dir clojure-project))
			     'face 'bold)))
      (insert 
       (mapconcat 'identity (clojure-project-packages clojure-project) "\n"))
      (setq inhibit-read-only nil)      
      (view-buffer buf))))

(defun clojure-project-create-package (&optional package project)
  "If no parameters are provided, interactively creates a package for the currently open project.
If parameters are present creates a package for a projectcas defined by parameters."
  (interactive)
  (let ((display-packages-p nil)
	(dir-part (lambda (st) (substring st 0 (string-match "\\.[^.]+$" st))))
	(file-part (lambda (st) (substring st (string-match "[^.]+$" st))))
	(dir)
	(file))
    (unless (and package project)
      (setq display-packages-p t)
      (setq project clojure-project)
      (clojure-project-show-packages)
      ; get package name
      (setq package 
	    (completing-read
	     (format "Creating a package for: %s\nEnter package name: "
		     (clojure-project-dir project))
	     ; completion with only directory part of existing packages
	     (mapcar 
	      (lambda (d) (concat (funcall dir-part d) "."))
	      (clojure-project-packages project)))))
    (setq dir
	  (concat (clojure-project-dir project) "/src/"
		  (replace-regexp-in-string "\\." "/" 
					    (funcall dir-part package) )))
    (make-directory dir t)
    (setq file
	  (concat dir "/" (funcall file-part package) ".clj" ))
    (with-temp-file file
      (insert (format "(ns %s)" package)))
    (if display-packages-p 
	(clojure-project-show-packages)) ; update package list
    (if clojure-project-open-after-creating-package-p
	(find-file-noselect file))))

(defun clojure-project-open-package (&optional package project)
  "If no parameters are provided, interactively opens a package for the currently open project.
If parameters are present opens a package for a project as defined by parameters."
  (interactive)
  (let ((file))
    (unless (and package project)
      (setq project clojure-project)
      (clojure-project-show-packages)
      ; get package name
      (setq package 
	    (completing-read
	     (format "Opening a package for: %s\nEnter package name: "
		     (clojure-project-dir project))
	     ; completion with existing packages
	     (clojure-project-packages project))))
    (setq file
	  (concat (clojure-project-dir project) "/src/"
		 (replace-regexp-in-string "\\." "/" package) ".clj" ))
    (find-file file)))

(provide 'clojure-project)




