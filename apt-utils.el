;;; apt-utils.el --- Emacs interface to APT (Debian package management)

;;; Copyright (C) 2002, 03 Matthew P. Hodges

;; Author: Matthew P. Hodges <matt@tc.bham.ac.uk>
;;	$Id$

;; apt-utils.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; apt-utils.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; Package to interface Emacs with APT. Start things off using e.g.:
;; M-x apt-utils-show-package RET emacs21 RET
;;
;; Other packages (dependencies, conflicts etc) can be navigated using
;; apt-utils-{next,previous}-package, apt-utils-choose-package-link or
;; apt-utils-follow-link. Return to the previous package with
;; apt-utils-view-previous-package. ChangeLog and README files for the
;; current package can easily be accessed with, for example,
;; apt-utils-view-changelog.
;;
;; For normal (i.e., not virtual) packages, the information can be
;; toggled between `package' and `showpkg' displays using
;; apt-utils-toggle-package-info; the latter is useful for the
;; "Reverse Depends".
;;
;; View the key bindings with describe-mode (bound to ? by default).

;;; Code:

(require 'cl)                           ; for set-difference

(unless (fboundp 'puthash)
  (if (fboundp 'cl-puthash)
      (defalias 'puthash 'cl-puthash)
    (error "No puthash function known")))

;; Customizable variables

(defcustom apt-utils-fill-packages t
  "*Fill APT package names if t."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-show-link-info t
  "*Show APT package descriptions when cycling through links if t."
  :group 'apt-utils
  :type 'boolean)

;; Faces

(defface apt-utils-normal-package-face
  '((((class color) (background light))
     (:foreground "purple"))
    (((class color) (background dark))
     (:foreground "yellow")))
  "Face used for APT hyperlinks."
  :group 'apt-utils)

(defface apt-utils-virtual-package-face
  '((((class color) (background light))
     (:foreground "green4"))
    (((class color) (background dark))
     (:foreground "green")))
  "Face used for APT hyperlinks."
  :group 'apt-utils)

;; Other variables

(defvar apt-utils-apt-cache-program "/usr/bin/apt-cache"
  "Location of the apt-cache program.")

(defvar apt-utils-dpkg-program "/usr/bin/dpkg"
  "Location of the dpkg program.")

(defvar apt-utils-grep-dctrl-program "/usr/bin/grep-dctrl"
  "Location of the grep-dctrl program.")

(defvar apt-utils-grep-dctrl-file-list
  (directory-files "/var/lib/apt/lists" t "_Packages")
  "List of files searched by `apt-utils-search-grep-dctrl'.")

(defvar apt-utils-package-list nil
  "List of packages known to APT.")

(defvar apt-utils-virtual-package-list nil
  "List of virtual packages known to APT.")

(defvar apt-utils-package-hashtable nil
  "Hash table containing APT packages types.")

(defvar apt-utils-package-lists-built nil
  "Whether or not APT package lists are built.")

(defvar apt-utils-current-packages nil
  "Packages associated with the *APT package info* buffer.")
(make-variable-buffer-local 'apt-utils-current-packages)

(defvar apt-utils-current-links nil
  "Package links associated with the *APT package info* buffer.")
(make-variable-buffer-local 'apt-utils-current-links)

(defvar apt-utils-buffer-positions nil
  "Cache of positions associated with current packages.
These are stored in a hash table.")
(make-variable-buffer-local 'apt-utils-buffer-positions)

(defvar apt-utils-dired-buffer nil
  "Keep track of dired buffer.")

;; XEmacs support

(defconst apt-utils-xemacs-p
  (or (featurep 'xemacs)
      (string-match "XEmacs\\|Lucid" (emacs-version)))
  "True if we are using apt-utils under XEmacs.")

;; Commands and functions

;;;###autoload
(defun apt-utils-show-package (&optional arg)
  "Write APT package information to buffer.
With ARG, choose that package, otherwise prompt for one."
  (interactive)
  (let ((buffer "*APT package info*")
        package type)
    ;; If ARG is provided, the car is the package name and the cdr the
    ;; package type
    (cond ((and (not (null arg)) (listp arg))
           (setq package (car arg))
           (setq type (cdr arg)))
          ((stringp arg)
           (setq package arg))
          (t
           (setq package (apt-utils-choose-package))))
    ;; Type might not be known yet
    (unless type
      (setq type (apt-utils-package-type package)))
    ;; Set up the buffer
    (if (get-buffer buffer)
        (set-buffer buffer)
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)
      (setq truncate-lines nil))
    ;; If called interactively, initialize apt-utils-current-packages
    (when (interactive-p)
      (setq apt-utils-current-packages (cons (cons package type) nil))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond
       ((equal type 'normal)
        (call-process apt-utils-apt-cache-program nil t nil "show" package)
        ;; Remove old versions
        (goto-char (point-min))
        (re-search-forward "^$")
        (unless (eobp)
          (delete-region (point) (point-max)))
        (apt-utils-add-package-links))
       ;; Virtual package or normal package w/ showpkg
       ((or (equal type 'virtual) (equal type 'normal-showpkg))
        (call-process apt-utils-apt-cache-program nil t nil "showpkg" package)
        (apt-utils-add-showpkg-links))
       ;; Normal search
       ((equal type 'search)
        (insert (format "Debian package search for %s\n\n" package))
        (call-process apt-utils-apt-cache-program nil t nil "search" package)
        (apt-utils-add-search-links))
       ;; Search for names only
       ((equal type 'search-names-only)
        (insert (format "Debian package search (names only) for %s\n\n" package))
        (call-process apt-utils-apt-cache-program nil t nil
                      "search" "--names-only" package)
        (apt-utils-add-search-links))
       ((equal type 'search-grep-dctrl)
        (insert (format "grep-dctrl search for %s\n\n"
                        (concat (format "\"%s\" " (car package))
                                (mapconcat 'identity (cdr package) " "))))
        (apply 'call-process apt-utils-grep-dctrl-program nil t nil package)
        (apt-utils-add-package-links))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (display-buffer buffer)))

(defun apt-utils-list-package-files ()
  "List the files associated with the current package.
Only works for installed packages; uses `apt-utils-dpkg-program'."
  (interactive)
  (let ((package (caar apt-utils-current-packages)) files)
    (with-temp-buffer
      (insert "(setq files '(\n")
      (call-process apt-utils-dpkg-program nil t nil "-L" package)
      (insert "))")
      ;; Check for files
      (cond
       ((or (search-backward "does not contain any files" nil t)
            (search-backward "is not installed" nil t))
        (message "Package does not contain any files/is not installed"))
       (t
        (eval-buffer)
        (setq files
              (delq nil
                    (mapcar (lambda (elt)
                              (if (or (file-regular-p (symbol-name elt))
                                      (string-equal "/." (symbol-name elt)))
                                  (symbol-name elt)
                                nil))
                            files)))
        ;; Some versions of Emacs won't update dired for the same
        ;; directory name if it already exists
        (if (buffer-live-p apt-utils-dired-buffer)
            (kill-buffer apt-utils-dired-buffer))
        (setq apt-utils-dired-buffer (dired-noselect files))
        (display-buffer apt-utils-dired-buffer))))))

(defun apt-utils-search (&optional arg)
  "Search Debian packages for regular expression.
With ARG, match names only."
  (interactive "p")
  (let ((buffer "*APT package info*")
        (regexp (read-from-minibuffer "Search packages for regexp: ")))
    ;; Set up the buffer
    (if (get-buffer buffer)
        (set-buffer buffer)
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)
      (setq truncate-lines nil))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Debian package search for %s\n\n" regexp))
      (cond
       (current-prefix-arg
        (call-process apt-utils-apt-cache-program nil t nil
                      "search" regexp "--names-only")
        (setq apt-utils-current-packages (cons (cons regexp 'search-names-only) nil)))
       (t
        (call-process apt-utils-apt-cache-program nil t nil "search" regexp)
        (setq apt-utils-current-packages (cons (cons regexp 'search) nil))))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal)))
      (apt-utils-add-search-links)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer buffer))))

(defun apt-utils-search-grep-dctrl ()
  "Search Debian packages for regular expression using grep-dctrl."
  (interactive)
  (let (args
        (buffer "*APT package info*")
        (fields (read-from-minibuffer "Search package fields: "))
        (show (read-from-minibuffer "Show package fields: "))
        (regexp (read-from-minibuffer "Search regexp: ")))
    (if (get-buffer buffer)
        (set-buffer buffer)
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)
      (setq truncate-lines nil))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Construct argument list (need to keep this)
      (setq args (append (list regexp fields show)
                         apt-utils-grep-dctrl-file-list))
      (insert (format "grep-dctrl search for %s\n\n"
                      (mapconcat 
                       (lambda (elt)
                         (if (string-equal regexp elt)
                             (format "\"%s\"" regexp)
                           elt))
                       args " ")))
      (apply 'call-process
             apt-utils-grep-dctrl-program nil t nil args)
      (setq apt-utils-current-packages (cons (cons args 'search-grep-dctrl) nil))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal)))
      (apt-utils-add-package-links)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer buffer))))

(defun apt-utils-toggle-package-info ()
  "Toggle between package and showpkg info for normal packages."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package (caar apt-utils-current-packages))
        (type (cdar apt-utils-current-packages))
        posn)
    (cond
     ((equal type 'normal)
      (setq posn (apt-utils-update-buffer-positions 'toggle))
      (setq apt-utils-current-packages
            (cons (cons package 'normal-showpkg)
                  (cdr apt-utils-current-packages)))
      (apt-utils-show-package (car apt-utils-current-packages))
      (goto-char posn))
     ((equal type 'normal-showpkg)
      (setq posn (apt-utils-update-buffer-positions 'toggle))
      (setq apt-utils-current-packages
            (cons (cons package 'normal)
                  (cdr apt-utils-current-packages)))
      (apt-utils-show-package (car apt-utils-current-packages))
      (goto-char posn))
     ((equal type 'virtual)
      (message "Cannot toggle info for virtual packages."))
     ((or (equal type 'search)
          (equal type 'search-names-only)
          (equal type 'search-grep-dctrl))
      (message "Cannot toggle info for searches.")))))

;; Find ChangeLog files

(defun apt-utils-view-changelog ()
  "Find ChangeLog for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer"))
   ((not (equal (cdar apt-utils-current-packages) 'normal))
    (message "Not a normal package"))
   (t
    (let ((package (caar apt-utils-current-packages)))
      (apt-utils-view-changelog-file package)))))
    
(defun apt-utils-view-changelog-file (package)
  "Find ChangeLog file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/changelog" package)
          '("" ".gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No ChangeLog file found for %s." package))))

;; Find Debian ChangeLog files

(defun apt-utils-view-debian-changelog ()
  "Find Debian ChangeLog for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer"))
   ((not (equal (cdar apt-utils-current-packages) 'normal))
    (message "Not a normal package"))
   (t
    (let ((package (caar apt-utils-current-packages)))
      (apt-utils-view-debian-changelog-file package)))))
    
(defun apt-utils-view-debian-changelog-file (package)
  "Find Debian ChangeLog file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/changelog.Debian" package)
          '("" ".gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No Debian ChangeLog file found for %s." package))))

;; Find README files

(defun apt-utils-view-readme ()
  "Find README for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer"))
   ((not (equal (cdar apt-utils-current-packages) 'normal))
    (message "Not a normal package"))
   (t
    (let ((package (caar apt-utils-current-packages)))
      (apt-utils-view-readme-file package)))))
    
(defun apt-utils-view-readme-file (package)
  "Find README file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/README" package)
          '("" ".gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No README file found for %s." package))))

;; Find Debian README files

(defun apt-utils-view-debian-readme ()
  "Find Debian README for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer"))
   ((not (equal (cdar apt-utils-current-packages) 'normal))
    (message "Not a normal package"))
   (t
    (let ((package (caar apt-utils-current-packages)))
      (apt-utils-view-debian-readme-file package)))))

(defun apt-utils-view-debian-readme-file (package)
  "Find Debian README file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/README" package)
          '(".Debian" ".Debian.gz" ".debian" ".debian.gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No Debian README file found for %s." package))))

(defun apt-utils-find-readable-file (root suffixes)
  "Find a readable file composed of ROOT and one of SUFFIXES."
  (catch 'found
    (mapcar (lambda (suffix)
              (when (file-readable-p (concat root suffix))
                (throw 'found (concat root suffix))))
            suffixes)
    nil))                               ; Return nil, if no file found

(defun apt-utils-view-file (file)
  "View ChangeLog or README information in FILE."
  (cond ((string-match "\\.gz$" file)
         (if (fboundp 'with-auto-compression-mode)
             (with-auto-compression-mode
               (view-file file))
           (auto-compression-mode 1)
           (view-file file)))
        (t
         (view-file file))))

;; Follow hyperlinks

(defun apt-utils-follow-link ()
  "Follow hyperlink at point."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package
         (cadr
          (member 'apt-package (text-properties-at (point))))))
    (apt-utils-follow-link-internal package)))

(defun apt-utils-mouse-follow-link (event)
  "Follow hyperlink at mouse click.
Argument EVENT is a mouse event."
  (interactive "e")
  (let (package posn)
    ;; Mouse may be in a different window, i.e. buffer
    (setq posn
          (cond
           ((fboundp 'posn-point)
            (posn-point (event-start event)))
           ((fboundp 'event-point)
            (event-point event))
           (t
            (error "Cannot determine event position"))))
    (set-buffer (window-buffer
                 (cond
                  ((fboundp 'posn-window)
                   (posn-window (event-start event)))
                  ((fboundp 'event-window)
                   (event-window event))
                  (t
                   (error "Cannot determine event window")))))
    (setq package
          (cadr
           (member 'apt-package (text-properties-at
                                 posn))))
    (apt-utils-follow-link-internal package)))

(defun apt-utils-follow-link-internal (package)
  "Follow hyperlink for PACKAGE."
  (if package
      (progn
        (apt-utils-update-buffer-positions 'forward)
        (apt-utils-show-package package)
        (setq apt-utils-current-packages
              (cons (cons package (apt-utils-package-type package))
                    apt-utils-current-packages)))
    (message "No known package at point.")))

;; Go to previous package in list

(defun apt-utils-view-previous-package ()
  "Go back to previous package displayed."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (if (cdr apt-utils-current-packages)
      (progn
        (let ((posn (apt-utils-update-buffer-positions 'backward)))
          (apt-utils-show-package (cadr apt-utils-current-packages))
          (goto-char posn))
        (setq apt-utils-current-packages (cdr apt-utils-current-packages)))
    (message "No previous packages.")))

;; Adapted from widget-move

(defun apt-utils-next-package (&optional arg)
  "Move point to the ARG next package.
ARG may be negative to move backward."
  (interactive "p")
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (cond
   ;; No links
   ((null apt-utils-current-links)
    (message "No package links."))
   ;; One link
   ((= (length apt-utils-current-links) 1)
    (goto-char (point-min))
    (goto-char (next-single-property-change (point)
                                                 'apt-package)))
   (t
    (let ((pos (point))
          (number arg)
          (old (apt-utils-package-at))
          new)
      ;; Forward.
      (while (> arg 0)
        (cond ((eobp)
               (goto-char (point-min)))
              (t
               (goto-char (or (next-single-property-change
                               (point) 'apt-package)
                              (point-max)))))
        (let ((new (apt-utils-package-at)))
          (when new
            (unless (eq new old)
              (setq arg (1- arg))
              (setq old new)))))
      ;; Backward.
      (while (< arg 0)
        (cond ((bobp)
               (goto-char (point-max)))
              (t
               (goto-char (or (previous-single-property-change
                               (point) 'apt-package)
                              (point-min)))))
        (let ((new (apt-utils-package-at)))
          (when new
            (unless (eq new old)
              (setq arg (1+ arg))))))
      ;; Go to beginning of field.
      (let ((new (apt-utils-package-at)))
        (while (eq (apt-utils-package-at) new)
          (backward-char)))
      (forward-char))))
  ;; Echo some info
  (when apt-utils-show-link-info
    (apt-utils-package-at-message)))

(defun apt-utils-previous-package (&optional arg)
  "Move point to the ARG previous package.
ARG may be negative to move forward."
  (interactive "p")
  (apt-utils-next-package (- arg)))

;; Choose a package from the known links

(defun apt-utils-choose-package-link ()
  "Choose a Debian package from the list of known links."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package
         (completing-read "Choose related Debian package: "
                          (mapcar (lambda (elt)
                                    (cons elt elt))
                                  apt-utils-current-links) nil t)))
    (when (> (length package) 0)
      (apt-utils-update-buffer-positions 'forward)
      (apt-utils-show-package package)
      (setq apt-utils-current-packages
            (cons (cons package (apt-utils-package-type package))
                  apt-utils-current-packages)))))

(defun apt-utils-package-list ()
  "Return list of known Debian packages."
  (unless apt-utils-package-lists-built
    (apt-utils-build-package-lists))
  apt-utils-package-list)

(defun apt-utils-virtual-package-list ()
  "Return list of known Debian packages."
  (unless apt-utils-package-lists-built
    (apt-utils-build-package-lists))
  apt-utils-virtual-package-list)

(defun apt-utils-build-package-lists (&optional force)
  "Build list of Debian packages known to APT.
With optional argument FORCE, rebuild list even if it is non-nil."
  (when (or force (null apt-utils-package-list))
    (unwind-protect
        (progn
          (setq apt-utils-package-lists-built nil)
          (message "Building Debian package lists...")
          ;; All packages except virtual ones
          (with-temp-buffer
            (insert "(setq apt-utils-package-list '(\n")
            (call-process apt-utils-apt-cache-program nil t nil "pkgnames"
                          ;; Don't get virtual packages
                          "-o" "APT::Cache::AllNames=0")
            (insert "))")
            (eval-buffer))
          ;; Virtual packages (difference between all, and all minus
          ;; virtual unfortunately)
          (with-temp-buffer
            (insert "(setq apt-utils-virtual-package-list '(\n")
            (call-process apt-utils-apt-cache-program nil t nil "pkgnames")
            (insert "))")
            (eval-buffer))
          ;; Find the difference
          (setq apt-utils-virtual-package-list
                (set-difference apt-utils-virtual-package-list
                                apt-utils-package-list))
          ;; Massage (for use with completing read)
          (setq apt-utils-package-list
                (mapcar (lambda (elt)
                          (cons (downcase (symbol-name elt)) nil))
                        apt-utils-package-list))
          (setq apt-utils-virtual-package-list
                (mapcar (lambda (elt)
                          (cons (downcase (symbol-name elt)) nil))
                        apt-utils-virtual-package-list))
          ;; Hash table listing package types
          (if (hash-table-p apt-utils-package-hashtable)
              (clrhash apt-utils-package-hashtable))
          (setq apt-utils-package-hashtable (make-hash-table :test 'equal))
          (mapcar (lambda (elt)
                    (puthash (downcase (car elt)) 'normal apt-utils-package-hashtable))
                  apt-utils-package-list)
          (mapcar (lambda (elt)
                    (puthash (downcase (car elt)) 'virtual apt-utils-package-hashtable))
                  apt-utils-virtual-package-list)
          (message "Building Debian package lists...done")
          (setq apt-utils-package-lists-built t))
      (unless apt-utils-package-lists-built
        (message "Building Debian package lists...interrupted")
        (setq apt-utils-package-list nil
              apt-utils-virtual-package-list nil)
        (if (hash-table-p apt-utils-package-hashtable)
            (clrhash apt-utils-package-hashtable))))))

(defun apt-utils-choose-package ()
  "Choose a Debian package from list."
  (completing-read "Choose Debian package: "
                   (append (apt-utils-package-list)
                         (apt-utils-virtual-package-list))
                   nil t))

;; Add hyperlinks

(defun apt-utils-add-package-links ()
  "Add hyperlinks to related Debian packages."
  (let ((keywords '("Conflicts" "Depends" "Pre-Depends" "Package"
                    "Provides" "Recommends" "Replaces" "Suggests"))
        keywords-maxlen regexp)
    (when apt-utils-fill-packages
      (setq keywords-maxlen
            (car
             (sort
              (mapcar
               (lambda (elt)
                 ;; Search for header
                 (goto-char (point-min))
                 (if (re-search-forward (concat "^" elt ": ") nil t)
                     (length elt)
                   0)
                 ) keywords) #'>))))
    (setq apt-utils-current-links nil)
    (while keywords
      (setq regexp (concat "^" (car keywords) ": "))
      (goto-char (point-min))
      (while (re-search-forward regexp (point-max) t)
        ;; Padding
        (when apt-utils-fill-packages
          (insert
           (make-string (- keywords-maxlen (length (car keywords))) ? )))
        ;; Find packages
        (let ((packages
               ;; Packages split by commas, or alternatives by
               ;; vertical bars
               (split-string (buffer-substring
                              (point)
                              ;; (line-end-position))
                              (save-excursion
                                (end-of-line)
                                (point)))
                             " ?[,|] ?"))
              (inhibit-read-only t)
              face
              length length-no-version
              package)
          (while packages
            (setq package (car packages))
            (setq length (length package))
            ;; Remove version info (in parenthesis), and whitespace
            (setq package
                  (cond
                   ((fboundp 'replace-regexp-in-string)
                    (replace-regexp-in-string "\\((.*)\\|\\s-+\\)" "" package))
                   ((fboundp 'replace-in-string)
                    (replace-in-string package "\\((.*)\\|\\s-+\\)" ""))
                   ((and (require 'dired)
                         (fboundp 'dired-replace-in-string))
                    (dired-replace-in-string "\\((.*)\\|\\s-+\\)" "" package))
                   (t
                    (error "No replace in string function"))))
            (setq length-no-version (length package))
            ;; Package type
            (cond
             ((equal (apt-utils-package-type package) 'normal)
              (setq face 'apt-utils-normal-package-face))
             ((equal (apt-utils-package-type package) 'virtual)
              (setq face 'apt-utils-virtual-package-face)))
            ;; Add text properties
            (add-text-properties (point) (+ (point) length-no-version)
                                 `(face ,face
                                        mouse-face highlight
                                        apt-package ,package))
            ;; Fill package names
            (and apt-utils-fill-packages
                 (> (current-column) (+ 2 keywords-maxlen))
                 (> (+ (current-column) length) fill-column)
                 (insert "\n" (make-string (+ 2 keywords-maxlen) ? )))
            ;; Store list of unique package links
            (unless (member package apt-utils-current-links)
              (setq apt-utils-current-links
                      (cons package apt-utils-current-links)))
            (forward-char length)
            (skip-chars-forward ", |")
            (setq packages (cdr packages)))))
      (setq keywords (cdr keywords)))))

(defun apt-utils-add-showpkg-links ()
  "Add hyperlinks to related Debian packages."
  (let ((keywords '("Reverse Depends" "Reverse Provides"))
        (inhibit-read-only t)
        start end regexp face link)
    (setq apt-utils-current-links nil)
    (while keywords
      (setq regexp (concat "^" (car keywords) ": "))
      (goto-char (point-min))
      (when (re-search-forward regexp (point-max) t)
        ;; Limits of search
        (setq start (1+ (point)))
        (setq end (or (re-search-forward "[a-z]:" (point-max) t)
                      (point-max)))
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (not (eobp))
            (when (or (looking-at "^\\s-+\\(.*\\),")
                      (looking-at "^\\(.*\\) "))
              (setq link (match-string 1))
              ;; Store list of unique package links
              (setq apt-utils-current-links
                    (cons link apt-utils-current-links))
              (cond
               ((equal (apt-utils-package-type link) 'normal)
                (setq face 'apt-utils-normal-package-face))
               ((equal (apt-utils-package-type link) 'virtual)
                (setq face 'apt-utils-virtual-package-face)))
              (add-text-properties (match-beginning 1) (match-end 1)
                                   `(face ,face
                                          mouse-face highlight
                                          apt-package ,link)))
            (forward-line))))
      (setq keywords (cdr keywords)))))

(defun apt-utils-add-search-links ()
  "Add hyperlinks to related Debian packages."
  (let ((inhibit-read-only t)
        start end face link)
    (setq apt-utils-current-links nil)
    (goto-char (point-min))
    (forward-line 2)                    ; Move past header
    (while (re-search-forward "^\\([^ ]+\\) - " (point-max) t)
      (setq link (match-string 1))
      ;; Store list of unique package links
      (when
          (not (member link apt-utils-current-links))
        (setq apt-utils-current-links
              (cons link apt-utils-current-links)))
      (cond
       ((equal (apt-utils-package-type link) 'normal)
        (setq face 'apt-utils-normal-package-face))
       ((equal (apt-utils-package-type link) 'virtual)
        (setq face 'apt-utils-virtual-package-face)))
      (add-text-properties (match-beginning 1) (match-end 1)
                           `(face ,face
                                  mouse-face highlight
                                  apt-package ,link)))))

(defun apt-utils-package-type (package)
  "Return what type of package PACKAGE is."
  (unless apt-utils-package-lists-built
    (apt-utils-build-package-lists))
  (or (gethash package apt-utils-package-hashtable)
      (error "Unknown type for package \"%s\"" package)))

(defun apt-utils-package-at ()
  "Get package at point."
  (get-text-property (point) 'apt-package))

(defun apt-utils-package-at-message ()
  "Emit message describing package at point."
  (let ((package (apt-utils-package-at)))
    (if package
        (with-temp-buffer
          (call-process apt-utils-apt-cache-program nil t nil "show" package)
          (if (re-search-backward "^Description: \\(.*\\)$" (point-min) t)
              (message "%s: %s" package (match-string 1))
            (message "%s: virtual package (no description)."
                     package))))))

(defun apt-utils-quit ()
  "Quit the *APT package info* buffer."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (if (fboundp 'quit-window)
      (quit-window)
    (bury-buffer)))

;; Track positions

(defun apt-utils-update-buffer-positions (type)
  "Update `apt-utils-buffer-positions'.
TYPE can be forward, backward, or toggle."
  (let (posn)
    (cond
     ((eq type 'forward)
      ;; Make the key unique; we could visit the same package more
      ;; than once
      (puthash (format "%s/%s/%d"
                       (caar apt-utils-current-packages)
                       (cdar apt-utils-current-packages)
                       (length apt-utils-current-packages))
               (point)
               apt-utils-buffer-positions))
     ((eq type 'backward)
      ;; Remove old values
      (remhash (format "%s/normal/%d"
                       (caar apt-utils-current-packages)
                       (length apt-utils-current-packages))
               apt-utils-buffer-positions)
      (remhash (format "%s/normal-showpkg/%d"
                       (caar apt-utils-current-packages)
                       (length apt-utils-current-packages))
               apt-utils-buffer-positions)
      (remhash (format "%s/virtual/%d"
                       (caar apt-utils-current-packages)
                       (length apt-utils-current-packages))
               apt-utils-buffer-positions)
      ;; Get position for previous package
      (setq posn
            (gethash (format "%s/%s/%d"
                             (caadr apt-utils-current-packages)
                             (cdadr apt-utils-current-packages)
                             (1- (length apt-utils-current-packages)))
                     apt-utils-buffer-positions)))
     ((eq type 'toggle)
      ;; new/old package types
      (let ((package (caar apt-utils-current-packages))
            (type (cdar apt-utils-current-packages))
            new old)
        (if (equal type 'normal)
            (setq old 'normal
                  new 'normal-showpkg)
          (setq old 'normal-showpkg
                new 'normal))
        ;; Set position for old entry
        (puthash (format "%s/%s/%d"
                         package
                         old
                         (length apt-utils-current-packages))
                 (point)
                 apt-utils-buffer-positions)
        ;; Get position for new entry
        (setq posn
              (gethash (format "%s/%s/%d"
                               package
                               new
                               (length apt-utils-current-packages))
                       apt-utils-buffer-positions
                       1))              ; default value
        )))
    posn))

;; Mode settings

(defvar apt-utils-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1")         'delete-other-windows)
    (define-key map (kbd "?")         'describe-mode)
    (define-key map (kbd "<")         'apt-utils-view-previous-package)
    (define-key map (kbd ">")         'apt-utils-choose-package-link)
    (define-key map (kbd "c")         'apt-utils-view-changelog)
    (define-key map (kbd "C")         'apt-utils-view-debian-changelog)
    (define-key map (kbd "q")         'apt-utils-quit)
    (define-key map (kbd "l")         'apt-utils-list-package-files)
    (define-key map (kbd "r")         'apt-utils-view-readme)
    (define-key map (kbd "R")         'apt-utils-view-debian-readme)
    (define-key map (kbd "s")         'apt-utils-show-package)
    (define-key map (kbd "S")         'apt-utils-search)
    (define-key map (kbd "t")         'apt-utils-toggle-package-info)
    (define-key map (kbd "SPC")       'scroll-up)
    (define-key map (kbd "DEL")       'scroll-down)
    (define-key map (kbd "RET")       'apt-utils-follow-link)
    (define-key map (kbd "TAB")       'apt-utils-next-package)
    (define-key map (kbd "M-TAB")     'apt-utils-previous-package)
    (if apt-utils-xemacs-p
        (define-key map '(button2) 'apt-utils-mouse-follow-link)
      (define-key map (kbd "<mouse-2>") 'apt-utils-mouse-follow-link))
    map)
  "Keymap for apt-utils mode.")

;; Menus

(defvar apt-utils-menu nil
  "Menu to use for `apt-utils-mode'.")

(when (fboundp 'easy-menu-define)

  (easy-menu-define apt-utils-menu apt-utils-mode-map "Apt Utils Menu"
    '("Apt Utils"
      "---"
      ["Show Package"          apt-utils-show-package t]
      ["View Previous Package" apt-utils-view-previous-package t]
      ["Choose Package Link"   apt-utils-choose-package-link t]
      ["Next Package"          apt-utils-next-package t]
      ["Previous Package"      apt-utils-previous-package t]
      ["Follow Link"           apt-utils-follow-link t]
      ["Search"                apt-utils-search t]
      "---"
      ["View ChangeLog"        apt-utils-view-changelog t]
      ["View Debian ChangeLog" apt-utils-view-debian-changelog t]
      ["View README"           apt-utils-view-readme t]
      ["View Debian README"    apt-utils-view-debian-readme t]
      "---"
      ["List Package Files"    apt-utils-list-package-files t]
      ["Toggle Package Info"   apt-utils-toggle-package-info t]
      "---"
      ["Quit"                  apt-utils-quit t])))

(defun apt-utils-mode ()
  "Major mode for controlling the *APT package info* buffer.

\\{apt-utils-mode-map}"
  (kill-all-local-variables)
  (use-local-map apt-utils-mode-map)
  (setq major-mode 'apt-utils-mode)
  (setq mode-name "APT utils")
  (when (and (fboundp 'easy-menu-add)
             apt-utils-menu)
    (easy-menu-add apt-utils-menu))
  (run-hooks 'apt-utils-mode-hook))

(provide 'apt-utils)

;;; apt-utils.el ends here
