;;; debian-el.el --- startup file for the debian-el package

;;; Commentary:
;; 
;; This file is loaded from /etc/emacs/site-start.d/50debian-el.el

;;; History:
;;
;; 2003-09-01 - Peter Galbraith
;;  - Created.

;;; Code:

(defgroup debian-el nil
  "Debian debian-el package customization."
  :group 'convenience)

(require 'debian-el-loaddefs)
;;(require 'debian-el-custom)

;; apt-sources
(add-to-list 'auto-mode-alist '("sources.list$" . apt-sources-mode))
(defgroup apt-sources nil "Mode for editing apt source.list file"
  :group 'tools
  :prefix "apt-sources-"
  :link '(custom-manual "(debian-el)apt-sources")
  :load 'apt-sources
;;:require 'apt-sources
  :group 'debian-el)
  
;; apt-utils
(defgroup apt-utils nil
  "Emacs interface to APT (Debian package management)"
  :group 'tools
  :link '(url-link "http://www.tc.bham.ac.uk/~matt/AptUtilsEl.html")
  :link '(custom-manual "(debian-el)apt-utils")
  :load 'apt-utils
;;:require 'apt-utils
  :group 'debian-el)

;; debian-bug.el
(defgroup debian-bug nil "Debian Bug report helper"
  :group 'tools
  :prefix "debian-bug-"
  :link '(custom-manual "(debian-el)debian-bug")
  :load 'debian-bug
;;:require 'debian-bug
  :group 'debian-el)

;; deb-view.el
(setq auto-mode-alist (append '(("\\.deb$" . deb-view-mode)) auto-mode-alist))
(defgroup deb-view nil
  "View Debian package files with tar-mode"
  :group 'tools
  :prefix "deb-view"
  :link '(custom-manual "(debian-el)deb-view")
  :load 'deb-view
  :group 'debian-el)
(add-hook
 'dired-load-hook
 (function (lambda ()
	     (define-key dired-mode-map "\C-d" 'deb-view-dired-view))))

(provide 'debian-el)

;;; debian-el.el ends here
