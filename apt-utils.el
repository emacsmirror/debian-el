;;; apt-utils.el --- Emacs interface to APT (Debian package management)

;;; Copyright (C) 2002, 2003, 2004 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
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
;; Other packages (dependencies, conflicts etc.) can be navigated
;; using apt-utils-{next,previous}-package,
;; apt-utils-choose-package-link or apt-utils-follow-link. Return to
;; the previous package with apt-utils-view-previous-package.
;; ChangeLog and README files for the current package can easily be
;; accessed with, for example, apt-utils-view-changelog.
;;
;; For normal (i.e., not virtual) packages, the information can be
;; toggled between `package' and `showpkg' displays using
;; apt-utils-toggle-package-info; the latter is useful for the
;; "Reverse Depends".
;;
;; View the key bindings with describe-mode (bound to ? by default).

;;; Code:

(require 'jka-compr)

(cond
 ((fboundp 'puthash)
  (defalias 'apt-utils-puthash 'puthash))
 ((and (require 'cl)
       (fboundp 'cl-puthash))
  (defalias 'apt-utils-puthash 'cl-puthash))
 (t
  (error "No puthash function known")))

;; Customizable variables

(defgroup apt-utils nil
  "Emacs interface to APT (Debian package management)."
  :group 'tools
  :link '(url-link "http://www.tc.bham.ac.uk/~matt/published/Public/AptUtilsEl.html"))

(defcustom apt-utils-fill-packages t
  "*Fill APT package names if t."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-show-link-info t
  "*Show APT package descriptions when cycling through links if t."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-show-all-versions nil
  "*Show APT descriptions for multiple package versions if t."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-automatic-update 'ask
  "*Controls automatic rebuilding of APT package lists.

If t always rebuilt when `apt-utils-timestamped-file' is newer than
`apt-utils-package-list-update-time'. If equal to the symbol ask,
ask the user about the update. If nil, never update automatically."
  :group 'apt-utils
  :type '(choice (const :tag "Always update automatically" t)
                 (const :tag "Ask user about update" ask)
                 (const :tag "Never update automatically" nil)))

(defcustom apt-utils-grep-dctrl-args '("-e")
  "*List of arguments to pass to `apt-utils-grep-dctrl-program'."
  :group 'apt-utils
  :type '(repeat string))

(defcustom apt-utils-mode-name-update nil
  "*String indicating that package lists need updating."
  :group 'apt-utils
  :type 'string)

(defcustom apt-utils-kill-buffer-confirmation-function 'yes-or-no-p
  "Function called before killing any buffers.
Used by `apt-utils-kill-other-window-buffers'."
  :group 'apt-utils
  :type '(choice (const :tag "Kill buffers only after yes or no query" yes-or-no-p)
                 (const :tag "Kill buffers only after y or n query" y-or-n-p)
                 (const :tag "Never kill buffers" ignore)
                 (const :tag "Kill buffers without confirmation" nil)))

;; Faces

(defface apt-utils-normal-package-face
  '((((class color) (background light))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "yellow")))
  "Face used for APT normal package hyperlinks."
  :group 'apt-utils)

(defface apt-utils-virtual-package-face
  '((((class color) (background light))
     (:foreground "green4"))
    (((class color) (background dark))
     (:foreground "green")))
  "Face used for APT virtual package hyperlinks."
  :group 'apt-utils)

(defface apt-utils-field-keyword-face
  '((((class color) (background light))
     (:foreground "purple" :bold t))
    (((class color) (background dark))
     (:foreground "purple" :bold t)))
  "Face used for APT field keywords."
  :group 'apt-utils)

(defface apt-utils-field-contents-face
  '((((class color) (background light))
     (:foreground "orchid"))
    (((class color) (background dark))
     (:foreground "orange")))
  "Face used for APT field contents."
  :group 'apt-utils)

(defface apt-utils-description-face
  '((((class color))
     (:foreground "cadet blue")))
  "Face used for APT package description."
  :group 'apt-utils)

(defface apt-utils-version-face
  '((((class color))
     (:italic t)))
  "Face used for APT package versions."
  :group 'apt-utils)

(defface apt-utils-broken-face
  '((((class color))
     (:foreground "red")))
  "Face used for unknown APT package."
  :group 'apt-utils)

;; Other variables

(defvar apt-utils-apt-cache-program "/usr/bin/apt-cache"
  "Location of the apt-cache program.")

(defvar apt-utils-dpkg-program
  (if (file-executable-p "/usr/bin/dlocate")
      "/usr/bin/dlocate"
    "/usr/bin/dpkg")
  "Location of the dpkg program.")

(defvar apt-utils-grep-dctrl-program "/usr/bin/grep-dctrl"
  "Location of the grep-dctrl program.")

(defvar apt-utils-grep-dctrl-file-list
  (directory-files "/var/lib/apt/lists" t "_Packages")
  "List of files searched by `apt-utils-search-grep-dctrl'.")

(defvar apt-utils-completion-table nil
   "List of packages known to APT; used by `completing-read'.
Only generated if `apt-utils-completing-read-hashtable-p' is
nil.")

(defvar apt-utils-package-hashtable nil
  "Hash table containing APT packages types.")

(defvar apt-utils-package-lists-built nil
  "Whether or not APT package lists are built.")

(defvar apt-utils-package-history nil
  "History of packages for each `apt-utils-mode' buffer.")
(make-variable-buffer-local 'apt-utils-package-history)

(defvar apt-utils-current-links nil
  "Package links associated with the `apt-utils-mode' buffer.")
(make-variable-buffer-local 'apt-utils-current-links)

(defvar apt-utils-buffer-positions nil
  "Cache of positions associated with package history.
These are stored in a hash table. See also
`apt-utils-package-history'")
(make-variable-buffer-local 'apt-utils-buffer-positions)

(defvar apt-utils-dired-buffer nil
  "Keep track of dired buffer.")

(defvar apt-utils-package-list-update-time nil
  "The time that `apt-utils-build-package-lists' was last done.")

(defvar apt-utils-automatic-update-asked nil
  "Non-nil if user already asked about updating package lists.")

(defvar apt-utils-timestamped-file "/var/cache/apt/pkgcache.bin"
  "File to check timestamp of (see `apt-utils-automatic-update').")

;; XEmacs support

(defconst apt-utils-xemacs-p
  (or (featurep 'xemacs)
      (string-match "XEmacs\\|Lucid" (emacs-version)))
  "True if we are using apt-utils under XEmacs.")

(if apt-utils-xemacs-p
    (progn
      (defalias 'apt-utils-line-end-position 'point-at-eol))
  (defalias 'apt-utils-line-end-position 'line-end-position))

;; Other version-dependent configuration

(defconst apt-utils-completing-read-hashtable-p
  (and (not apt-utils-xemacs-p)
       (or
        ;; Next released version after 21.3 will support this
        (and
         (>= emacs-major-version 21)
         (>= emacs-minor-version 4))
        (>= emacs-major-version 22)
        ;; As will the current pretest
        (string-match "\\..*\\..*\\." emacs-version)))
  "Non-nil if `completing-read' supports hash table as input.")

(defconst apt-utils-face-property
  (cond
   ((and (not apt-utils-xemacs-p)
	 (or
	  ;; Next released version after 21.3 will support this
	  (and
	   (>= emacs-major-version 21)
	   (>= emacs-minor-version 4))
	  (>= emacs-major-version 22)
	  ;; As will the current pretest
	  (string-match "\\..*\\..*\\." emacs-version)))
    'font-lock-face)
   (t
    'face))
  "Use font-lock-face if `add-text-properties' supports it.
Otherwise, just use face.")

(cond
 ;; Emacs 21
 ((fboundp 'replace-regexp-in-string)
  (defalias 'apt-utils-replace-regexp-in-string 'replace-regexp-in-string))
 ;; Emacs 20
 ((and (require 'dired)
       (fboundp 'dired-replace-in-string))
  (defalias 'apt-utils-replace-regexp-in-string 'dired-replace-in-string))
 ;; XEmacs
 ((fboundp 'replace-in-string)
  (defun apt-utils-replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))
 ;; Bail out
 (t
  (error "No replace in string function found")))

;; Commands and functions

;;;###autoload
(defun apt-utils-show-package ()
  "Show information for a Debian package.
A selection of known packages is presented. See `apt-utils-mode'
for more detailed help."
  (interactive)
  (apt-utils-show-package-1 t))

(defun apt-utils-show-package-1 (&optional interactive package-spec new-session)
  "Present Debian package information in a dedicated buffer.

If INTERACTIVE is non-nil, then we have been called
interactively (or from a keyboard macro) via
`apt-utils-show-package'. Hence, reset the history of visited
packages.

If PACKAGE-SPEC is specified, this can either be a string (the
name of the package) or a list, where the car of the list is the
name of the package, and the cdr is the package type; if not
specified, a package name is prompted for. If NEW-SESSION is
non-nil, generate a new `apt-utils-mode' buffer."
  (apt-utils-check-package-lists)
  (let (package type)
    (cond ((and package-spec (listp package-spec))
           (setq package (car package-spec))
           (setq type (cdr package-spec)))
          ((stringp package-spec)
           (setq package package-spec))
          (t
           (setq package (apt-utils-choose-package))))
    ;; Type might not be known yet
    (unless type
      (setq type (apt-utils-package-type package)))
    ;; Set up the buffer
    (cond
     (new-session
      (set-buffer (generate-new-buffer "*APT package info*"))
      (apt-utils-mode))
     ((eq major-mode 'apt-utils-mode)
      ;; do nothing
      )
     (t
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)))
    ;; If called interactively, initialize apt-utils-package-history
    (when (or interactive new-session)
      (setq apt-utils-package-history (cons (cons package type) nil))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond
       ((equal type 'normal)
        (call-process apt-utils-apt-cache-program nil '(t nil) nil "show" package)
        ;; Remove old versions if not wanted
        (unless apt-utils-show-all-versions
          (goto-char (point-min))
          (re-search-forward "^$")
          (unless (eobp)
            (delete-region (point) (point-max))))
        (apt-utils-add-package-links))
       ;; Virtual package or normal package w/ showpkg
       ((or (equal type 'virtual) (equal type 'normal-showpkg))
        (call-process apt-utils-apt-cache-program nil '(t nil) nil "showpkg" package)
        (apt-utils-add-showpkg-links))
       ;; Normal search
       ((equal type 'search)
        (insert (format "Debian package search for %s\n\n" package))
        (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
               "search" "--" (split-string package "&&"))
        (apt-utils-add-search-links 'search))
       ;; Search for names only
       ((equal type 'search-names-only)
        (insert (format "Debian package search (names only) for %s\n\n" package))
        (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
               "search" "--names-only" "--" (split-string package "&&"))
        (apt-utils-add-search-links 'search-names-only))
       ;; Search for file names
       ((equal type 'search-file-names)
        (insert (format "Debian package search (file names) for %s\n\n" package))
        (apply 'call-process apt-utils-dpkg-program nil t nil
               "-S" (list package))
        (apt-utils-add-search-links 'search-file-names))
       ;; grep-dctrl search
       ((equal type 'search-grep-dctrl)
        (insert (format "grep-dctrl search for %s\n\n"
                        (concat (format "\"%s\" " (car package))
                                (mapconcat 'identity (cdr package) " "))))
        (apply 'call-process apt-utils-grep-dctrl-program nil t nil package)
        (apt-utils-add-package-links))))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    ;; The point and window-start only need setting for new sessions
    ;; or when choosing new packages with apt-utils-follow-link or
    ;; apt-utils-choose-package-link.
    (goto-char (point-min))
    (set-window-start (display-buffer (current-buffer)) (point-min))))

(defun apt-utils-list-package-files ()
  "List the files associated with the current package.
The list appears in a `dired-mode' buffer. Only works for
installed packages; uses `apt-utils-dpkg-program'."
  (interactive)
  (let ((package (caar apt-utils-package-history))
        (type (cdar apt-utils-package-history))
        files)
    (setq files (apt-utils-get-package-files package))
    (cond
     ((or (equal type 'normal) (equal type 'normal-showpkg))
      (if files
          (progn
            ;; Some versions of Emacs won't update dired for the same
            ;; directory name if it already exists
            (if (buffer-live-p apt-utils-dired-buffer)
                (kill-buffer apt-utils-dired-buffer))
            (setq apt-utils-dired-buffer (dired-noselect files))
            (display-buffer apt-utils-dired-buffer))
        (message "Package does not contain any files/is not installed.")))
     (t
      (message "No files associated for type: %s." type)))))

(defun apt-utils-get-package-files (package &optional filter)
  "Return a list of files belonging to package PACKAGE.
With optional argument FILTER, return files matching this regular
expression."
  (let (files)
    (with-temp-buffer
      (call-process apt-utils-dpkg-program nil t nil "-L" package)
      ;; Check for files
      (cond
       ((or (search-backward "does not contain any files" nil t)
            (search-backward "not installed" nil t)))
       (t
        (goto-char (point-min))
        (insert "(setq files '(\n")
        (while (re-search-forward "^\\(.+\\)$" nil t)
          (replace-match "\"\\1\""))
        (insert "))")
        (eval-buffer)
        ;; Keep regular files or top directory (for dired)
        (setq files
              (delq nil
                    (mapcar (lambda (elt)
                              (if (and (or (file-regular-p elt)
                                           (string-equal "/." elt))
                                       (string-match (or filter ".") elt))
                                  elt
                                nil))
                            files))))))
    files))

;;;###autoload
(defun apt-utils-search ()
  "Search Debian packages for regular expression.
To search for multiple patterns use a string like \"foo&&bar\"."
  (interactive)
  (apt-utils-search-internal 'search))

(defun apt-utils-search-names-only ()
  "Search Debian package names for regular expression.
To search for multiple patterns use a string like \"foo&&bar\"."
  (interactive)
  (apt-utils-search-internal 'search-names-only))

(defun apt-utils-search-file-names ()
  "Search Debian file names for string."
  (interactive)
  (apt-utils-search-internal 'search-file-names))

(defun apt-utils-search-internal (type)
  "Search Debian packages for regular expression or string.
The type of search is specified by TYPE."
  (apt-utils-check-package-lists)
  (let ((regexp (read-from-minibuffer
                 (cond ((eq type 'search-file-names)
                        "Search file names for string: ")
                       (t
                        "Search packages for regexp: ")))))
    ;; Set up the buffer
    (cond
     ((eq major-mode 'apt-utils-mode)
      ;; do nothing
      )
     (t
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Can't search for string starting with "-" because the "--"
      ;; option isn't understood by dpkg or dlocate
      (when (and (eq type 'search-file-names)
                 (string-match "^-" regexp))
        (setq regexp (apt-utils-replace-regexp-in-string "^-+" "" regexp)))
      (insert (format "Debian package search%s for %s\n\n"
                      (cond ((eq type 'search-names-only) " (names only)")
                            ((eq type 'search-file-names) " (file names)")
                            (t ""))
                      regexp))
      (cond
       ((eq type 'search)
        (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
               "search" "--" (split-string regexp "&&"))
        (setq apt-utils-package-history (cons (cons regexp 'search) nil)))
       ((eq type 'search-names-only)
        (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
               "search" "--names-only" "--" (split-string regexp "&&"))
        (setq apt-utils-package-history (cons (cons regexp 'search-names-only) nil)))

       ((eq type 'search-file-names)
        (apply 'call-process apt-utils-dpkg-program nil t nil
               "-S" (list regexp))
        (setq apt-utils-package-history (cons (cons regexp 'search-file-names) nil))))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal)))
      (apt-utils-add-search-links type)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer (current-buffer)))))

(defun apt-utils-search-grep-dctrl ()
  "Search Debian packages for regular expression using grep-dctrl."
  (interactive)
  (apt-utils-check-package-lists)
  (let (args
        (fields (apt-utils-read-fields "Search package fields: "))
        (show (apt-utils-read-fields "Show package fields: "))
        (regexp (read-from-minibuffer "Search regexp: ")))
    ;; Check args
    (cond
     ((equal (length fields) 0)
      (error "No fields selected for search"))
     ((equal (length show) 0)
      (error "No fields selected for show"))
     ((equal (length regexp) 0)
      (error "No regexp selected")))
    (setq fields (concat "-F" fields))
    (setq show (concat "-s" show))
    (cond
     ((eq major-mode 'apt-utils-mode)
      ;; do nothing
      )
     (t
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Construct argument list (need to keep this)
      (setq args (append apt-utils-grep-dctrl-args (list regexp fields show)
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
      (setq apt-utils-package-history (cons (cons args 'search-grep-dctrl) nil))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal)))
      (apt-utils-add-package-links)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer (current-buffer)))))

(defun apt-utils-read-fields (prompt)
  "Read fields for `apt-utils-search-grep-dctrl'.
Use PROMPT for `completing-read'."
  (let ((chosen "foo")
        (completion-ignore-case t)
        ;; Why can't I use '(...) for the list?
        (keywords (list "Architecture" "Bugs" "Conffiles" "Conflicts"
                        "Depends" "Description" "Enhances" "Essential"
                        "Filename" "Installed-Size" "MD5sum" "Maintainer"
                        "Origin" "Package" "Pre-Depends" "Priority"
                        "Provides" "Recommends" "Replaces" "Section"
                        "Size" "Source" "Suggests" "Task" "Version" "url"))
        fields)
    (while (> (length chosen) 0)
        (setq chosen
              (completing-read prompt
                               (mapcar (lambda (elt)
                                         (list elt elt))
                                       keywords)
                               nil
                               t))
      (setq keywords (delete chosen keywords))
      (if (stringp fields)
          (progn
            (when (> (length chosen) 0)
              (setq fields (concat fields "," chosen))))
        (setq fields chosen)))
    fields))

(defun apt-utils-toggle-package-info ()
  "Toggle between package and showpkg info for normal packages."
  (interactive)
  (apt-utils-check-package-lists)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package (caar apt-utils-package-history))
        (type (cdar apt-utils-package-history))
        posns)
    (cond
     ((equal type 'normal)
      (setq posns (apt-utils-update-buffer-positions 'toggle))
      (setq apt-utils-package-history
            (cons (cons package 'normal-showpkg)
                  (cdr apt-utils-package-history)))
      (apt-utils-show-package-1 nil (car apt-utils-package-history))
      (goto-char (car posns))
      (set-window-start (selected-window) (cadr posns)))
     ((equal type 'normal-showpkg)
      (setq posns (apt-utils-update-buffer-positions 'toggle))
      (setq apt-utils-package-history
            (cons (cons package 'normal)
                  (cdr apt-utils-package-history)))
      (apt-utils-show-package-1 nil (car apt-utils-package-history))
      (goto-char (car posns))
      (set-window-start (selected-window) (cadr posns)))
     ((equal type 'virtual)
      (message "Cannot toggle info for virtual packages."))
     ((or (equal type 'search)
          (equal type 'search-names-only)
          (equal type 'search-file-names)
          (equal type 'search-grep-dctrl))
      (message "Cannot toggle info for searches.")))))

(defun apt-utils-check-package-lists ()
  "Determine whether package lists need rebuilding."
  (cond
   ((null apt-utils-package-lists-built)
    (apt-utils-build-package-lists))
   ((and (apt-utils-time-less-p apt-utils-package-list-update-time
                      (nth 5 (file-attributes apt-utils-timestamped-file)))
         ;; Only act for non-nil apt-utils-automatic-update
         apt-utils-automatic-update
         (cond
          ((eq apt-utils-automatic-update t))
          ((eq apt-utils-automatic-update 'ask)
           (unless apt-utils-automatic-update-asked
             (when (eq major-mode 'apt-utils-mode)
               (setq mode-name (concat "APT utils" apt-utils-mode-name-update)))
             (setq apt-utils-automatic-update-asked t)
             (yes-or-no-p
              "APT package lists may be out of date. Update them? ")))))
    (apt-utils-build-package-lists t))))

;; Find ChangeLog files

(defun apt-utils-view-changelog ()
  "Find ChangeLog for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history)))
      (apt-utils-view-changelog-file package)))))

(defun apt-utils-view-changelog-file (package)
  "Find ChangeLog file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("CHANGELOG" "ChangeLog" "Changelog" "changelog")
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
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history)))
      (apt-utils-view-debian-changelog-file package)))))

(defun apt-utils-view-debian-changelog-file (package)
  "Find Debian ChangeLog file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("changelog.Debian")
          '(".gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No Debian ChangeLog file found for %s." package))))

;; Find NEWS files

(defun apt-utils-view-news ()
  "Find NEWS for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history)))
      (apt-utils-view-news-file package)))))

(defun apt-utils-view-news-file (package)
  "Find NEWS file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("NEWS")
          '("" ".gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No NEWS file found for %s." package))))

;; Find Debian NEWS files

(defun apt-utils-view-debian-news ()
  "Find Debian NEWS for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history)))
      (apt-utils-view-debian-news-file package)))))

(defun apt-utils-view-debian-news-file (package)
  "Find Debian NEWS file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("NEWS.Debian")
          '(".gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No Debian NEWS file found for %s." package))))

;; Find README files

(defun apt-utils-view-readme ()
  "Find README for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history)))
      (apt-utils-view-readme-file package)))))

(defun apt-utils-view-readme-file (package)
  "Find README file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("README")
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
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history)))
      (apt-utils-view-debian-readme-file package)))))

(defun apt-utils-view-debian-readme-file (package)
  "Find Debian README file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("README.Debian" "README.debian")
          '("" ".gz"))))
    (if file
        (apt-utils-view-file file)
      (message "No Debian README file found for %s." package))))

;; Find copyright files

(defun apt-utils-view-copyright ()
  "Find copyright file for current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history)))
      (apt-utils-view-copyright-file package)))))

(defun apt-utils-view-copyright-file (package)
  "Find copyright file for PACKAGE."
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/copyright" package)
          '("")
          '(""))))
    (if file
        (apt-utils-view-file file)
      (message "No copyright file found for %s." package))))

(defun apt-utils-view-man-page ()
  "View man page for the current package.
If there is more than one man page associated with the package,
offer a choice."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (equal (cdar apt-utils-package-history) 'normal))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history))
          (regexp
           "^.*/man/\\([a-zA-Z_/]+\\)?man[0-9]/\\(.*\\)\\.\\([0-9a-z]+\\)\\.gz")
          choice chosen files table)
      (setq files (apt-utils-get-package-files package
                                               "/man/.*\\.gz$"))
      (cond
       ((null files)
        (message "No man pages found for %s." package))
       ((not (cdr files))
        (setq chosen (car files)))
       (t
        (setq table (mapcar
                     (lambda (file)
                       (setq choice
                             (with-temp-buffer
                               (insert file)
                               (re-search-backward regexp nil t)
                               (replace-match "\\2 (\\1\\3)" nil nil)
                               (buffer-string)))
                       (cons choice file))
                     files))
        (setq chosen
              (cdr (assoc
                    (let ((completion-ignore-case t))
                      (completing-read "Choose man page: " table nil t))
                    table)))))
      (when chosen
        (if (fboundp 'woman-find-file)
            (woman-find-file chosen)
          (manual-entry chosen)))))))

;; File-related utility functions

(defun apt-utils-find-readable-file (dir prefixes suffixes)
  "Find a readable file composed of directory prefix and suffix.
Directory is DIR, prefix is one of PREFIXES and suffix is one of
SUFFIXES."
  (catch 'found
    (mapcar (lambda (prefix)
              (mapcar (lambda (suffix)
                        (when (file-readable-p (concat dir prefix suffix))
                          (throw 'found (concat dir prefix suffix))))
                      suffixes))
            prefixes)
    nil))                               ; Return nil, if no file found

(defun apt-utils-view-file (file)
  "View file FILE in function `view-mode'."
  (cond ((string-match "\\.gz$" file)
         (if (fboundp 'with-auto-compression-mode)
             (with-auto-compression-mode
               (view-file file))
           (auto-compression-mode 1)
           (view-file file)))
        (t
         (view-file file))))

;; Follow hyperlinks

(defun apt-utils-follow-link (new-session)
  "Follow hyperlink at point.
With non-nil NEW-SESSION, follow link in a new buffer."
  (interactive "P")
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package
         (cadr
          (member 'apt-package (text-properties-at (point))))))
    (apt-utils-follow-link-internal package new-session)))

(defun apt-utils-mouse-follow-link (event)
  "Follow hyperlink at mouse click.
Argument EVENT is a mouse event."
  (interactive "e")
  (let (package)
    (save-selected-window
      (mouse-set-point event)
      (setq package
            (cadr
             (member 'apt-package (text-properties-at
                                   (point)))))
      (apt-utils-follow-link-internal package nil))))

(defun apt-utils-follow-link-internal (package new-session)
  "Follow hyperlink for PACKAGE.
With non-nil NEW-SESSION, follow link in a new buffer."
  (cond
   ((equal package 'broken)
    (message "Package name is broken somehow."))
   (package
    (unless new-session
      (apt-utils-update-buffer-positions 'forward))
    (apt-utils-show-package-1 nil package new-session)
    (unless new-session
      (setq apt-utils-package-history
            (cons (cons package (apt-utils-package-type package))
                  apt-utils-package-history))))
   (t
    (message "No known package at point."))))

;; Go to previous package in list

(defun apt-utils-view-previous-package ()
  "Go back to previous package displayed."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (if (cdr apt-utils-package-history)
      (progn
        (let ((posns (apt-utils-update-buffer-positions 'backward)))
          (apt-utils-show-package-1 nil (cadr apt-utils-package-history))
          (goto-char (car posns))
          (set-window-start (selected-window) (cadr posns)))
        (setq apt-utils-package-history (cdr apt-utils-package-history)))
    (message "No further package history.")))

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
    (let ((old (apt-utils-package-at)))
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

(defun apt-utils-choose-package-link (new-session)
  "Choose a Debian package from a list of links.
With non-nil NEW-SESSION, follow link in a new buffer."
  (interactive "P")
  (apt-utils-choose-package-link-internal new-session))

(defun apt-utils-choose-package-link-internal (new-session)
  "Choose a Debian package from a list of links.
With non-nil NEW-SESSION, follow link in a new buffer."
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package
         (completing-read "Choose related Debian package: "
                          (mapcar (lambda (elt)
                                    (cons elt elt))
                                  apt-utils-current-links) nil t)))
    (when (> (length package) 0)
      (unless new-session
        (apt-utils-update-buffer-positions 'forward))
      (apt-utils-show-package-1 nil package new-session)
      (unless new-session
        (setq apt-utils-package-history
              (cons (cons package (apt-utils-package-type package))
                    apt-utils-package-history))))))

(defun apt-utils-build-package-lists (&optional force)
  "Build list of Debian packages known to APT.
With optional argument FORCE, rebuild the packages lists even if
they are defined. When package lists are not up-to-date, this is
indicated in `mode-name'."
  (when (or force (null apt-utils-package-lists-built))
    (unwind-protect
        (progn
          (setq apt-utils-package-lists-built nil
                apt-utils-package-list-update-time nil
                apt-utils-automatic-update-asked nil
                apt-utils-completion-table nil)
          (message "Building Debian package lists...")
          ;; Hash table listing package types
          (if (hash-table-p apt-utils-package-hashtable)
              (clrhash apt-utils-package-hashtable)
            (setq apt-utils-package-hashtable (make-hash-table :test 'equal)))
          ;; All packages except virtual ones
          (with-temp-buffer
            ;; Virtual and normal packages
            (call-process apt-utils-apt-cache-program nil '(t nil) nil "pkgnames")
            (mapcar (lambda (elt)
                      (apt-utils-puthash elt 'virtual apt-utils-package-hashtable))
                    (split-string (buffer-string)))
            ;; Normal packages
            (erase-buffer)
            (call-process apt-utils-apt-cache-program nil '(t nil) nil "pkgnames"
                          "-o" "APT::Cache::AllNames=0")
            (mapcar (lambda (elt)
                      (apt-utils-puthash elt 'normal apt-utils-package-hashtable))
                    (split-string (buffer-string))))
          (message "Building Debian package lists...done.")
          (setq apt-utils-package-lists-built t
                apt-utils-package-list-update-time
                (nth 5 (file-attributes apt-utils-timestamped-file)))
          (when (eq major-mode 'apt-utils-mode)
            (setq mode-name "APT utils")))
      (unless apt-utils-package-lists-built
        (message "Building Debian package lists...interrupted.")
        (when (eq major-mode 'apt-utils-mode)
          (setq mode-name (concat "APT utils" apt-utils-mode-name-update)))
        (if (hash-table-p apt-utils-package-hashtable)
            (clrhash apt-utils-package-hashtable))))))

(defun apt-utils-rebuild-package-lists ()
  "Rebuild the APT package lists."
  (interactive)
  (apt-utils-build-package-lists t))

(defun apt-utils-choose-package ()
  "Choose a Debian package name."
  (let ((package
         (and (eq major-mode 'apt-utils-mode)
              (cadr (member 'apt-package
                            (text-properties-at (point)))))))
    (completing-read "Choose Debian package: "
                     (cond
                      (apt-utils-completing-read-hashtable-p
                       apt-utils-package-hashtable)
                      (t
                       (or apt-utils-completion-table
                           (apt-utils-build-completion-table))))
                     nil t package)))

(defun apt-utils-build-completion-table ()
  "Build completion table for packages.
See `apt-utils-completion-table'."
  (with-temp-buffer
    (maphash (lambda (key value)
               (insert key "\n"))
             apt-utils-package-hashtable)
    (setq apt-utils-completion-table
          (mapcar (lambda (elt)
                    (list elt))
                  (split-string (buffer-string))))))

;; Add hyperlinks

(defun apt-utils-add-package-links ()
  "Add hyperlinks to related Debian packages."
  (let ((keywords '("Conflicts" "Depends" "Enhances" "Package"
                    "Pre-Depends" "Provides" "Recommends" "Replaces"
                    "Suggests"))
        match)
    (setq apt-utils-current-links nil)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ \n:]+\\):\\( \\|$\\)"
                              (point-max) t)
      (setq match (match-string 1))
      (add-text-properties (if (looking-at "$")
                               (point) ;; Conffiles (also see below)
                             (1- (point)))
                           (save-excursion
                             (beginning-of-line)
                             (point))
                           `(,apt-utils-face-property apt-utils-field-keyword-face))
      (cond
       ((member match keywords)
        ;; Remove newline characters in field
        (let ((end (apt-field-end-position)))
          (subst-char-in-region (point) end ?\n ?\  )
          (canonically-space-region (point) end))
        ;; Find packages
        (let ((packages (apt-utils-current-field-packages))
              (inhibit-read-only t)
              face
              length length-no-version
              package)
          (while packages
            (setq package (car packages))
            (setq length (length package))
            ;; Remove version info (in parenthesis), and whitespace
            (setq package (apt-utils-replace-regexp-in-string
                           "\\((.*)\\|\\s-+\\)" "" package))
            (setq length-no-version (length package))
            ;; Package type
            (cond
             ((equal (apt-utils-package-type package t) 'normal)
              (setq face 'apt-utils-normal-package-face))
             ((equal (apt-utils-package-type package t) 'virtual)
              (setq face 'apt-utils-virtual-package-face))
             (t
              (setq face 'apt-utils-broken-face)
              (setq package 'broken)))
            ;; Add text properties
            (add-text-properties (point) (+ (point) length-no-version)
                                 `(,apt-utils-face-property ,face
                                        mouse-face highlight
                                        apt-package ,package))
            ;; Version?
            (when (> length length-no-version)
              (add-text-properties (+ (point) length-no-version 1)
                                   (+ (point) length)
                                   `(,apt-utils-face-property apt-utils-version-face)))
            ;; Fill package names
            (when (and apt-utils-fill-packages
                       (> (current-column) (+ 2 (length match)))
                       (> (+ (current-column) length) fill-column))
              (when (equal (char-before) ?\ )
                (delete-char -1))          ; trailing whitespace
              (insert "\n" (make-string (+ 2 (length match)) ? )))
            ;; Store list of unique package links
            (unless (member package apt-utils-current-links)
              (setq apt-utils-current-links
                    (cons package apt-utils-current-links)))
            (forward-char length)
            (skip-chars-forward ", |\n")
            (setq packages (cdr packages)))))
       ((equal match "Description")
        (add-text-properties (point)
                             (save-excursion
                               (or
                                (re-search-forward "^[^ ]" (point-max) t)
                                (point-max)))
                             `(,apt-utils-face-property apt-utils-description-face)))
       ;; Conffiles doesn't have trailing space
       ((looking-at "$")
        nil)
       (t
        (add-text-properties (1- (point))
                             (save-excursion
                               (end-of-line)
                               (point))
                             `(,apt-utils-face-property apt-utils-field-contents-face)))))))

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
        (add-text-properties (match-beginning 0) (1- (match-end 0))
                             `(,apt-utils-face-property
                               apt-utils-field-keyword-face))
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
              (unless (member link apt-utils-current-links)
                (setq apt-utils-current-links
                      (cons link apt-utils-current-links)))
              (cond
               ((equal (apt-utils-package-type link t) 'normal)
                (setq face 'apt-utils-normal-package-face))
               ((equal (apt-utils-package-type link t) 'virtual)
                (setq face 'apt-utils-virtual-package-face))
               (t
                (setq face 'apt-utils-broken-face)
                (setq link 'broken)))
              (add-text-properties (match-beginning 1) (match-end 1)
                                   `(,apt-utils-face-property ,face
                                          mouse-face highlight
                                          apt-package ,link)))
          (forward-line))))
      (setq keywords (cdr keywords)))))

(defun apt-utils-add-search-links (type)
  "Add hyperlinks to related Debian packages.
The type of search is specified by TYPE."
  (let ((inhibit-read-only t)
        face link regexp)
    (setq apt-utils-current-links nil)
    (goto-char (point-min))
    (forward-line 2)                    ; Move past header
    (cond
     ((eq type 'search-file-names)
      ;; Reformat diversion information
      (save-excursion
        (while (re-search-forward "diversion by \\(.*\\) \\(from\\|to\\): \\(.*\\)" nil t)
          (replace-match "\\1: \\3 (diversion \\2)" nil nil)))
      (setq regexp "\\([^:,]+\\)[,:]"))
     (t
      (setq regexp"^\\([^ ]+\\) - ")))
    (while (re-search-forward regexp (point-max) t)
      (setq link (match-string 1))
      ;; Store list of unique package links
      (unless (member link apt-utils-current-links)
        (setq apt-utils-current-links
              (cons link apt-utils-current-links)))
      (cond
       ((equal (apt-utils-package-type link t) 'normal)
        (setq face 'apt-utils-normal-package-face))
       ((equal (apt-utils-package-type link t) 'virtual)
        (setq face 'apt-utils-virtual-package-face))
       (t
        (setq face 'apt-utils-broken-face)
        (setq link 'broken)))
      (add-text-properties (match-beginning 1) (match-end 1)
                           `(,apt-utils-face-property ,face
                                  mouse-face highlight
                                  apt-package ,link))
      ;; Multiple fields separated by commas
      (when (eq type 'search-file-names)
        (if (eq (char-before) ?\:)
            (goto-char (1+ (apt-utils-line-end-position)))
          (skip-chars-forward ", "))))))

(defun apt-utils-package-type (package &optional no-error)
  "Return what type of package PACKAGE is.
With optional argument NO-ERROR, don't flag an error for unknown
packages."
  (or (gethash package apt-utils-package-hashtable)
      (cond
       (no-error
        nil)
       (t
        (error
         (substitute-command-keys
          "Package name is broken: rebuild package lists using \\[apt-utils-rebuild-package-lists] may help")
         package)))))

(defun apt-utils-package-at ()
  "Get package at point."
  (get-text-property (point) 'apt-package))

(defun apt-utils-package-at-message ()
  "Emit message describing package at point."
  (let ((package (apt-utils-package-at)))
    (cond
     ((equal package 'broken)
      (message "Package name is broken somehow."))
     (package
      (with-temp-buffer
        (call-process apt-utils-apt-cache-program nil t nil "show" package)
        (if (re-search-backward "^Description: \\(.*\\)$" (point-min) t)
            (message "%s: %s." package (match-string 1))
          (message "%s: virtual package (no description)."
                   package)))))))

(defun apt-utils-quit ()
  "Quit this `apt-utils-mode' buffer."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (if (fboundp 'quit-window)
      (quit-window)
    (bury-buffer)))

(defun apt-utils-kill-buffer ()
  "Kill this `apt-utils-mode' buffer.
Also clean up `apt-utils-package-hashtable' and
`apt-utils-completion-table' using `apt-utils-cleanup'."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (kill-buffer (current-buffer))
  (apt-utils-cleanup))

(defun apt-utils-cleanup ()
  "Clean up lists used by `apt-utils-mode'.
Specifically, nullify `apt-utils-package-hashtable' and
`apt-utils-completion-table'. Only do this if there are no
buffers left in `apt-utils-mode'."
  (unless (memq 'apt-utils-mode
                (mapcar (lambda (b)
                          (with-current-buffer b
                            major-mode))
                        (buffer-list)))
    (clrhash apt-utils-package-hashtable)
    (setq apt-utils-completion-table nil
          apt-utils-package-lists-built nil)))

(defun apt-utils-describe-package ()
  "Describe package at point."
  (interactive)
  (apt-utils-package-at-message))

(defun apt-utils-kill-other-window-buffers ()
  "Kill buffers in other windows and the windows themselves.
See `apt-utils-kill-buffer-confirmation-function' for
customisation options."
  (interactive)
  (unless (eq major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (when (or (null apt-utils-kill-buffer-confirmation-function)
            (apply apt-utils-kill-buffer-confirmation-function
                   (list "Kill buffers in other windows? ")))
    (let ((buffer-list
           (delq (current-buffer)
                 (mapcar #'window-buffer (window-list)))))
      (mapc (lambda (b)
              (when (buffer-live-p b)
                (kill-buffer b)))
            buffer-list))
    (delete-other-windows))
  (message nil))

;; Track positions

(defun apt-utils-update-buffer-positions (type)
  "Update `apt-utils-buffer-positions'.
TYPE can be forward, backward, or toggle."
  (let (posns)
    (cond
     ((eq type 'forward)
      ;; Make the key unique; we could visit the same package more
      ;; than once
      (apt-utils-puthash (format "%s/%s/%d"
                       (caar apt-utils-package-history)
                       (cdar apt-utils-package-history)
                       (length apt-utils-package-history))
               (list (point) (window-start (selected-window)))
               apt-utils-buffer-positions))
     ((eq type 'backward)
      ;; Remove old values
      (remhash (format "%s/normal/%d"
                       (caar apt-utils-package-history)
                       (length apt-utils-package-history))
               apt-utils-buffer-positions)
      (remhash (format "%s/normal-showpkg/%d"
                       (caar apt-utils-package-history)
                       (length apt-utils-package-history))
               apt-utils-buffer-positions)
      (remhash (format "%s/virtual/%d"
                       (caar apt-utils-package-history)
                       (length apt-utils-package-history))
               apt-utils-buffer-positions)
      ;; Get position for previous package
      (setq posns
            (gethash (format "%s/%s/%d"
                             (car (cadr apt-utils-package-history))
                             (cdr (cadr apt-utils-package-history))
                             (1- (length apt-utils-package-history)))
                     apt-utils-buffer-positions)))
     ((eq type 'toggle)
      ;; new/old package types
      (let ((package (caar apt-utils-package-history))
            (type (cdar apt-utils-package-history))
            new old)
        (if (equal type 'normal)
            (setq old 'normal
                  new 'normal-showpkg)
          (setq old 'normal-showpkg
                new 'normal))
        ;; Set position for old entry
        (apt-utils-puthash (format "%s/%s/%d"
                         package
                         old
                         (length apt-utils-package-history))
                 (list (point) (window-start (selected-window)))
                 apt-utils-buffer-positions)
        ;; Get position for new entry
        (setq posns
              (gethash (format "%s/%s/%d"
                               package
                               new
                               (length apt-utils-package-history))
                       apt-utils-buffer-positions
                       (list 1 1)))     ; default value
        )))
    posns))

(defun apt-utils-current-field-packages ()
  "Return a list of the packages on the current line."
  (let ((keywords '("Conflicts" "Depends" "Enhances" "Package"
                    "Pre-Depends" "Provides" "Recommends" "Replaces"
                    "Suggests"))
        eol match packages posn string)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (cond
       ((eobp)
        (message "Not on package field line.")
        nil)
       ((and (re-search-forward "^\\([^ \n:]+\\): " eol t)
             (setq match (match-string 1))
             (member match keywords))
        (setq posn (point))
        (goto-char (apt-field-end-position))
        (setq string (buffer-substring-no-properties posn (point)))
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (while (re-search-forward "\n *" nil t)
            (replace-match " "))
          (setq packages
                ;; Packages split by commas, or alternatives by vertical
                ;; bars; for Enhances, multiple lines my be spanned
                (split-string (buffer-substring (point-min) (point-max))
                              " ?[,|] ?")))
        packages)
       (t
        (message "Not on package field line.")
        nil)))))

(defun apt-field-end-position ()
  "Move to end of current field."
  (save-excursion
    (re-search-forward "\\(^[^: ]+:\\|^$\\)")
    (beginning-of-line)
    (backward-char)
    (point)))

;; Borrowed from gnus/lisp/time-date.el

(defun apt-utils-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

;; Mode settings

(defvar apt-utils-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "#")             'apt-utils-rebuild-package-lists)
    (define-key map (kbd "1")             'delete-other-windows)
    (define-key map (kbd "<")             'apt-utils-view-previous-package)
    (define-key map (kbd ">")             'apt-utils-choose-package-link)
    (define-key map (kbd "?")             'describe-mode)
    (define-key map (kbd "C")             'apt-utils-view-debian-changelog)
    (define-key map (kbd "DEL")           'scroll-down)
    (define-key map (kbd "M-TAB")         'apt-utils-previous-package)
    (define-key map (kbd "N")             'apt-utils-view-debian-news)
    (define-key map (kbd "Q")             'apt-utils-kill-buffer)
    (define-key map (kbd "R")             'apt-utils-view-debian-readme)
    (define-key map (kbd "RET")           'apt-utils-follow-link)
    (define-key map (kbd "S")             'apt-utils-search)
    (define-key map (kbd "SPC")           'scroll-up)
    (define-key map (kbd "TAB")           'apt-utils-next-package)
    (define-key map (kbd "c")             'apt-utils-view-changelog)
    (define-key map (kbd "d")             'apt-utils-describe-package)
    (define-key map (kbd "f")             'apt-utils-search-file-names)
    (define-key map (kbd "g")             'apt-utils-search-grep-dctrl)
    (define-key map (kbd "i")             'apt-utils-view-copyright)
    (when (fboundp 'window-list)
      (define-key map (kbd "k")           'apt-utils-kill-other-window-buffers))
    (define-key map (kbd "l")             'apt-utils-list-package-files)
    (define-key map (kbd "m")             'apt-utils-view-man-page)
    (define-key map (kbd "n")             'apt-utils-view-news)
    (define-key map (kbd "o")             'apt-utils-search-names-only)
    (define-key map (kbd "q")             'apt-utils-quit)
    (define-key map (kbd "r")             'apt-utils-view-readme)
    (define-key map (kbd "s")             'apt-utils-show-package)
    (define-key map (kbd "t")             'apt-utils-toggle-package-info)
    (define-key map [(shift iso-lefttab)] 'apt-utils-previous-package)
    (define-key map [(shift tab)]         'apt-utils-previous-package)
    (define-key map
      (if apt-utils-xemacs-p '(button2) (kbd "<mouse-2>"))
      'apt-utils-mouse-follow-link)
    map)
  "Keymap for apt-utils mode.")

(unless apt-utils-mode-name-update
  (setq apt-utils-mode-name-update
        (substitute-command-keys
         ": update using \\<apt-utils-mode-map>\\[apt-utils-rebuild-package-lists]")))

;; Menus

(defvar apt-utils-menu nil
  "Menu to use for `apt-utils-mode'.")

(when (fboundp 'easy-menu-define)

  (easy-menu-define apt-utils-menu apt-utils-mode-map "Apt Utils Menu"
    '("Apt Utils"
      "---"
      ["Show Package"               apt-utils-show-package t]
      ["Toggle Package Info"        apt-utils-toggle-package-info t]
      ["View Previous Package"      apt-utils-view-previous-package t]
      ["Choose Package Link"        apt-utils-choose-package-link t]
      ["Next Package"               apt-utils-next-package t]
      ["Previous Package"           apt-utils-previous-package t]
      ["Follow Link"                apt-utils-follow-link t]
      ["List Package Files"         apt-utils-list-package-files t]
      "---"
      ["Search"                     apt-utils-search t]
      ["Search (names only)"        apt-utils-search-names-only t]
      ["Search (file names)"        apt-utils-search-file-names t]
      ["Search (grep-dctrl)"        apt-utils-search-grep-dctrl t]
      "---"
      ["View ChangeLog"             apt-utils-view-changelog t]
      ["View Debian ChangeLog"      apt-utils-view-debian-changelog t]
      ["View README"                apt-utils-view-readme t]
      ["View Debian README"         apt-utils-view-debian-readme t]
      ["View NEWS"                  apt-utils-view-news t]
      ["View Debian NEWS"           apt-utils-view-debian-news t]
      ["View copyright"             apt-utils-view-copyright t]
      ["View man page"              apt-utils-view-man-page t]
      "---"
      ["Rebuild Package Lists"      apt-utils-rebuild-package-lists t]
      "---"
      ["Quit"                       apt-utils-quit t]
      ["Kill Buffer"                apt-utils-kill-buffer t])))

(defun apt-utils-mode ()
  "Major mode to interface Emacs with APT (Debian package management).

Start things off with, for example:

    M-x apt-utils-show-package RET emacs21 RET

Other packages (dependencies, conflicts etc.) can be navigated
using:

    \\[apt-utils-next-package] move to next package link
    \\[apt-utils-previous-package] move to previous package link
    \\[apt-utils-choose-package-link] choose next package from current links
    \\[apt-utils-follow-link] show package for the link at point
    \\[apt-utils-view-previous-package] show the previous package from history
    \\[apt-utils-toggle-package-info] toggle package and showpkg information

Files associated with installed packages can be accessed using:

    \\[apt-utils-list-package-files] list package files (in a `dired' buffer)
    \\[apt-utils-view-changelog] view ChangeLog file
    \\[apt-utils-view-debian-changelog] view Debian ChangeLog file
    \\[apt-utils-view-readme] view README file
    \\[apt-utils-view-debian-readme] view Debian ChangeLog file
    \\[apt-utils-view-news] view NEWS file
    \\[apt-utils-view-debian-news] view Debian NEWS file
    \\[apt-utils-view-copyright] view copyright file
    \\[apt-utils-view-man-page] view man page

Package searchs can be performed using:

    \\[apt-utils-search] search for regular expression in package names and descriptions
    \\[apt-utils-search-names-only] search for regular expression in package names
    \\[apt-utils-search-grep-dctrl] search for regular expression in selected package fields
    \\[apt-utils-search-file-names] search for string in filenames

A history of navigated packages is maintained when package links
are followed using `apt-utils-choose-package-link' or
`apt-utils-follow-link'. This history is reset when
`apt-utils-show-package' or any of the search commands is used.

Key definitions:
\\{apt-utils-mode-map}"
  (kill-all-local-variables)
  (use-local-map apt-utils-mode-map)
  (setq major-mode 'apt-utils-mode)
  (setq mode-name "APT utils")
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  ;; XEmacs
  (when (and (fboundp 'easy-menu-add)
             apt-utils-menu)
    (easy-menu-add apt-utils-menu))
  (run-hooks 'apt-utils-mode-hook))

(provide 'apt-utils)

;;; apt-utils.el ends here
