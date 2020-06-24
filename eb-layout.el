;;; eb-layout.el --- Manage eyebrowse layouts -*- lexical-binding: t -*-

;; Author: Daniel Pritchett
;; Maintainer: Daniel Pritchett
;; Version: 0.1.0
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'dash)
(require 'hydra)
(require 'eyebrowse)

(defmacro comment (&rest body) nil)

(defconst ebl--layout-path "~/.emacs.d/eb-layouts")

(defvar ebl-saved-layouts)

;; TODO: auto-saving when user executes command
;; (defvar ebl-autosave-on-update nil)

(defvar ebl-layout-display-function)
(defvar ebl-layout-name)

;; Flow 1: basic CRUD for eyebrowse layout
(defun ebl--get-current-layout () (eyebrowse--get 'window-configs))

(defun ebl--set-eb-layout (cfg) (eyebrowse--set 'window-configs cfg))

;; (defun ebl--update-eb-layout (update-fn)
;;   (ebl--set-eb-layout (funcall update-fn (ebl--get-current-layout))))

(defun ebl--get-current-slot () (eyebrowse--get 'current-slot))

(defun ebl--set-current-slot (slot) (eyebrowse--set 'current-slot slot))

;; (defun ebl--update-current-slot (update-fn)
;;   (ebl--set-current-slot (funcall update-fn (ebl--get-current-slot))))

(defun ebl--list-saved-layout-names ()
  "List of saved layout by name."
  (seq-map #'car ebl-saved-layouts))

(defun ebl--add-layout-to-list (name layout)
  "Add or update the current layout to the list of saved layouts with name `name'." 
  (if (assoc name ebl-saved-layouts)
      (setf (alist-get name ebl-saved-layouts nil nil #'equal) (ebl--get-current-layout))
    (add-to-list 'ebl-saved-layouts (cons name (ebl--get-current-layout)))))

;; (map 'list #'car ebl-saved-layouts)

;; Flow 2 : Serialize Configs
(defun ebl--save-layout-to-file (layout &optional filename)
  (with-temp-file (or filename ebl--layout-path) 
    (prin1 ebl-saved-layouts (current-buffer))))

(defun ebl--load-layout-from-file ()
  (with-temp-buffer
    (insert-file-contents ebl--layout-path)
    (goto-char (point-min))
    (read (current-buffer))))

(defun ebl--layout-prompt ()
  (-let [prompt (concatenate 'string
			     "Choose layout ["
			     (string-join  (ebl--list-saved-layout-names) ", ")
			     "]: ")]
    (read-from-minibuffer prompt)))

(defun ebl--prompt-layout-name ()
  (-let [layouts (cl-map 'list
			 #'car
			 (ebl--load-layout-from-file))]
    (ivy-read "Enter Layout Name: " layouts)))

(defun ebl-save-current-layout (name)
  "Saves the current layout, adding entry having car `name'."
  (interactive (list (ebl--prompt-layout-name)))
  (-let [layout (ebl--get-current-layout)]
    (ebl--add-layout-to-list name layout)
    (ebl--save-layout-to-file layout)
    (setq ebl-layout-name name)))

(defun ebl-load-layout (name)
  "Load layout from file"
  (interactive (list (ebl--prompt-layout-name)))
  (if (not (file-exists-p ebl--layout-path))
      (message "There are no saved files!")
    (-let [layouts (ebl--load-layout-from-file)]
      ;; Set layout value
      (setq ebl-saved-layouts layouts)
      (setq ebl-layout-name name)
      ;; Set layout file
      (ebl--set-eb-layout (alist-get name layouts nil nil #'equal)))))

;; (-let [layouts (ebl--load-layout-from-file)]
;;   (alist-get "main" layouts nil nil #'equal))

;; Flow 3 Minipulating 
;; * Move Left and write - maintaining order of alist and updating slot number
;; * Remove one, maintining order
;; TODO: Insert before a specific slot number
;; TODO: Autosave flag
(defun ebl--swap-layout-entry (entry-a entry-b list)
  (seq-let [first &rest rest] list
    (cond ((null list) nil)
	  ((equal entry-a first)
	   (cons (cons (car entry-a)
		       (cdr entry-b))
		 (ebl--swap-layout-entry entry-a entry-b rest)))
	  ((equal entry-b first)
	   (cons (cons (car entry-b)
		       (cdr entry-a))
		 (ebl--swap-layout-entry entry-a entry-b rest)))
	  (t (cons first (ebl--swap-layout-entry entry-a entry-b rest))))))

(defun ebl--shift-position (should-shift? from-slot to-slot)
  (let ((cur-layout (ebl--get-current-layout)))
    (if should-shift?
	(let* ((entry-a (assoc from-slot cur-layout))
	       (entry-b (assoc to-slot cur-layout))
	       (swapped-list (ebl--swap-layout-entry entry-a entry-b cur-layout)))
	  (ebl--set-eb-layout swapped-list)
	  (ebl--set-current-slot to-slot)))))

(defun ebl-move-left ()
  (interactive)
  (let ((cur-slot (ebl--get-current-slot)))
    (ebl--shift-position (/= cur-slot 1) cur-slot (1- cur-slot))))

(defun ebl-move-right ()
  (interactive)
  (let ((cur-slot (ebl--get-current-slot)))
    (ebl--shift-position (assoc (1+ cur-slot) (ebl--get-current-layout))
			 cur-slot (1+ cur-slot))))

(defun ebl--balance-layouts (layout removed-slot)
  (seq-map (lambda (elem)
	     (seq-let [slot &rest layout] elem
	       (if (< removed-slot slot)
		   (cons (1- slot) layout)
		 elem)))
	   layout))

;;TODO Not switching on balance
(defun ebl-delete-current-layout ()
  (interactive)
  "Delete a layout and balance if necessary."
  (let ((cur-slot (ebl--get-current-slot)))
    (if (/= 0 cur-slot) ;; TODO: 0th layout is special and outside the balancing
	(if (assoc (1+ cur-slot) (ebl--get-current-layout)) ;; balance if not the last
	    ;; Balancing will not require changinging index
	    (progn
	      (eyebrowse--delete-window-config cur-slot)
	      (ebl--set-eb-layout (ebl--balance-layouts (ebl--get-current-layout) cur-slot))
	      (ebl--set-current-slot cur-slot))
	  (progn
	    (eyebrowse-prev-window-config 1)
	    (eyebrowse--delete-window-config cur-slot))))))

;; Hydra
(defun ebl-hydra//eyebrowse-list ()
  (mapcar 
   (lambda (cfg) 
     (let* ((config-num (car cfg))
	    (config-name (caddr cfg)))
       (cons config-num config-name)))
   (eyebrowse--get 'window-configs)))

(defun ebl-hydra//sel-formatter (cfg-str)
  (propertize (format "[ %s ]" cfg-str)
	      'face '(bold 'warning)))

(defun ebl-hydra//format-eyebrowse-config (sel-formatter-f)
  (let ((cur-slot (eyebrowse--get 'current-slot)))
    (mapcar
     (lambda (config)
       (let* ((config-idx (car config))
	      (config-name (cdr config))
	      (config-str (if (string-empty-p config-name)
			      (number-to-string config-idx)
			    (format "%s:%s" config-idx config-name))))
	 (if (eq cur-slot config-idx)
	     (concat (funcall sel-formatter-f config-str))
	   config-str)))
     (ebl-hydra//eyebrowse-list))))

(defhydra ebl-hydra-nav (:hint nil)
  "
%s(string-join (ebl-hydra//format-eyebrowse-config #'ebl-hydra//sel-formatter) \" | \")
^^^^ _n_: _n_ext           _0_: window config _0_  _5_: window config _5_
^^^^ _p_: _p_rev           _1_: window config _1_  _6_: window config _6_
^^^^ _c_: _c_reate config  _2_: window config _2_  _7_: window config _7_
^^^^ _D_: _D_elete config  _3_: window config _3_  _8_: window config _8_
^^^^ _r_: _r_ename config  _4_: window config _4_  _9_: window config _9_
^^^^ _S_: _S_ave config    _L_: _L_oad config      _q_:_q_uit            " 
  ("n" #'eyebrowse-next-window-config)
  ("p" #'eyebrowse-prev-window-config)
  ("TAB" #'eyebrowse-last-window-config)
  ("c" #'eyebrowse-create-window-config)
  ("D" #'eyebrowse-close-window-config)
  ("r" #'eyebrowse-rename-window-config)
  ("0" #'eyebrowse-switch-to-window-config-0)
  ("1" #'eyebrowse-switch-to-window-config-1)
  ("2" #'eyebrowse-switch-to-window-config-2)
  ("3" #'eyebrowse-switch-to-window-config-3)
  ("4" #'eyebrowse-switch-to-window-config-4)
  ("5" #'eyebrowse-switch-to-window-config-5)
  ("6" #'eyebrowse-switch-to-window-config-6)
  ("7" #'eyebrowse-switch-to-window-config-7)
  ("8" #'eyebrowse-switch-to-window-config-8)
  ("9" #'eyebrowse-switch-to-window-config-9)
  ("S" #'ebl-save-current-layout)
  ("L" #'ebl-load-layout)
  ("<" #'ebl-move-left)
  (">" #'ebl-move-right)
  ("q" nil :color blue))

(hydra-set-property 'ebl-hydra-nav :verbosity 1)

;;TODO: Insertion, not out of place (no 1 2 4 5) etc.  Keep balanced
(comment 
 ;;updating in place alist value
 (let* ((test-list '((a . 1)
		     (b . 2)))
	(b-elem (assoc 'b test-list)))
   ;; (setf (alist-get 'b test-list nil nil #'equal) 12)
   (setf (car b-elem) 'c)
   test-list
   ;; (setf (alist-get 'b test-list nil nil #'equal))
   ;; test-list
   )

 (y-or-n-p "Value: ")
 (ebl-save-layout "Original")
 (prin1 '(1 2 3))
 (let ((a nil))
   (setq a (plist-put a 'a 'b))
   a)
 (plist-put '(a c) 'a 'b)
 (plist-member '("test" 'name) "test")
 (let ((s (read-from "'(1 2 3)")))
   (car (eval s)))
 (eval  (read-from-string "(1 2 3)")))

(provide 'eb-layout)
;;; eb-layout.el ends here
