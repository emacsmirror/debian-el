;;; debian-el-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:

(provide 'debian-el-loaddefs)

;;;### (autoloads (apt-utils-search apt-utils-show-package) "apt-utils"
;;;;;;  "apt-utils.el" (16120 61272))
;;; Generated autoloads from apt-utils.el

(autoload (quote apt-utils-show-package) "apt-utils" "\
Write APT package information to buffer.
With ARG, choose that package, otherwise prompt for one." t nil)

(autoload (quote apt-utils-search) "apt-utils" "\
Search Debian packages for regular expression.
To search for multiple patterns use a string like \"foo&&bar\"." t nil)

;;;***

;;;### (autoloads (debian-bug debian-bug-get-bug-as-email debian-bug-get-bug-as-file
;;;;;;  debian-bug-web-package debian-bug-web-packages debian-bug-web-this-bug-under-mouse
;;;;;;  debian-bug-web-this-bug debian-bug-web-bug debian-bug-web-bugs
;;;;;;  debian-bug-intent-to-package debian-bug-request-for-package
;;;;;;  debian-bug-wnpp) "debian-bug" "debian-bug.el" (16248 63225))
;;; Generated autoloads from debian-bug.el

(autoload (quote debian-bug-wnpp) "debian-bug" "\
Submit a WNPP bug report to Debian.
Optional argument ACTION can be provided in programs." t nil)

(autoload (quote debian-bug-request-for-package) "debian-bug" "\
Shortcut for `debian-bug-wnpp' with RFP action." t nil)

(autoload (quote debian-bug-intent-to-package) "debian-bug" "\
Shortcut for `debian-bug-wnpp' with ITP action (for Debian developers)." t nil)

(autoload (quote debian-bug-web-bugs) "debian-bug" "\
Browse the BTS for this package via `browse-url'." t nil)

(autoload (quote debian-bug-web-bug) "debian-bug" "\
Browse the BTS for BUG-NUMBER via `browse-url'." t nil)

(autoload (quote debian-bug-web-this-bug) "debian-bug" "\
Browse the BTS via `browse-url' for the bug report number under point." t nil)

(autoload (quote debian-bug-web-this-bug-under-mouse) "debian-bug" "\
Browse the BTS via `browse-url' for the bug report number under mouse.
In a program, mouse location is in EVENT." t nil)

(autoload (quote debian-bug-web-packages) "debian-bug" "\
Search Debian web page for this package via `browse-url'." t nil)

(autoload (quote debian-bug-web-package) "debian-bug" "\
Search Debian web page in ARCHIVE for this package via `browse-url'." t nil)

(autoload (quote debian-bug-get-bug-as-file) "debian-bug" "\
Read bug report #BUG-NUMBER as a regular file." t nil)

(autoload (quote debian-bug-get-bug-as-email) "debian-bug" "\
Read bug report #BUG-NUMBER via Email interface." t nil)

(autoload (quote debian-bug) "debian-bug" "\
Submit a Debian bug report." t nil)

;;;***

;;;### (autoloads (apt-sources-mode) "apt-sources" "apt-sources.el"
;;;;;;  (16064 13921))
;;; Generated autoloads from apt-sources.el

(autoload (quote apt-sources-mode) "apt-sources" "\
Major mode for editing apt's sources.list file.
Sets up command `font-lock-mode'.

\\{apt-sources-mode-map}" t nil)

;;;***
