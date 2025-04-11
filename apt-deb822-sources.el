;;; apt-deb822-sources.el --- Mode for editing deb822-style APT source files. -*- lexical-binding:t -*-

;; Copyright 2025 Xiyue Deng <manphiz@gmail.com>

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL.

;;; Commentary:

;; This mode is for editing the APT source files that uses deb822-style
;; format. This new type of APT source files is supported since Debian Trixie,
;; usually located under `/etc/apt/sources.list.d/<foo>.sources'.  The old
;; format is supported by apt-sources.el.

;;; Code:

(defgroup apt-deb822-sources nil
  "Mode for editing deb822-style apt source files."
  :group 'tools
  :prefix "apt-deb822-sources-")

(defcustom apt-deb822-sources-mode-hook nil
  "Normal hooks run when entering `apt-deb822-sources-mode'."
  :type 'hook)

(defvar apt-deb822-sources-mode-map nil
  "Keymap for apt-deb822-sources-mode.")

(defvar apt-deb822-sources-mode-syntax-table nil
  "Syntax table for apt-deb822-sources-mode.")

(defvar apt-deb822-sources-mode-font-lock-keywords nil
  "Regexps to highlight in font-lock.")

(defvar apt-deb822-sources-mode--field-names
  '("Enabled"
    "Types"
    "URIs"
    "Suites"
    "Components"
    "Options"
    "Signed-By"
    "X-Repolib-Name")
  "Supported field names in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--enabled
  '("yes" "no")
  "Supported enabled status in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--types
  '("deb-src" "deb")
  "Supported types in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--debian-suites
  '("unstable" "sid"
    "experimental"
    "testing"
    "frozen"
    "stable"
    "buzz"
    "rex"
    "bo"
    "hamm"
    "slink"
    "potato"
    "woody"
    "sarge"
    "etch"
    "lenny"
    "squeeze"
    "wheezy"
    "jessie"
    "stretch"
    "buster"
    "bullseye"
    "bookworm"
    "trixie"
    "forky"
    "duke")
  "Supported Debian suites in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--ubuntu-suites
  '("warty"
    "hoary"
    "breezy"
    "dapper"
    "edgy"
    "feisty"
    "gutsy"
    "hardy"
    "intrepid"
    "jaunty"
    "karmic"
    "lucid"
    "maverick"
    "natty"
    "oneiric"
    "precise"
    "quantal"
    "raring"
    "saucy"
    "trusty"
    "utopic"
    "vivid"
    "wily"
    "xenial"
    "yakkety"
    "zesty"
    "artful"
    "bionic"
    "cosmic"
    "disco"
    "eoan"
    "focal"
    "groovy"
    "hirsute"
    "impish"
    "jammy"
    "kinetic"
    "lunar"
    "mantic"
    "noble"
    "oracular"
    "plucky")
  "Supported Ubuntu suites in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--suites
  (append
   apt-deb822-sources-mode--debian-suites
   apt-deb822-sources-mode--ubuntu-suites)
  "All supported suites in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--debian-components
  '("main"
    "contrib"
    "non-free-firmware"
    "non-free")
  "Supported components in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--ubuntu-components
  '("main"
    "universe"
    "multiverse"
    "restricted")
  "Supported components in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--components
  (append
   apt-deb822-sources-mode--debian-components
   apt-deb822-sources-mode--ubuntu-components)
  "All supported components in deb822-style APT source files.")

(defvar apt-deb822-sources-mode--uris
  '("https?"
    "ftps?"
    "file")
  "Supported URI types in deb822-style APT source files.")

(unless apt-deb822-sources-mode-syntax-table
  (setq apt-deb822-sources-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " apt-deb822-sources-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " apt-deb822-sources-mode-syntax-table)
  (modify-syntax-entry ?' "w   " apt-deb822-sources-mode-syntax-table))

(defun apt-deb822-sources-mode--font-lock-add-comments ()
  (add-to-list 'apt-deb822-sources-mode-font-lock-keywords
               '("#.*$" . font-lock-comment-face)))

(defun apt-deb822-sources-mode--font-lock-add-enabled (enabled-statuses)
  (dolist (enabled-status enabled-statuses)
    (add-to-list 'apt-deb822-sources-mode-font-lock-keywords
                 `(,(concat "^Enabled:.*\\_<\\(" enabled-status "\\)\\_>")
                   (1 font-lock-builtin-face)))))

(defun apt-deb822-sources-mode--font-lock-add-uris (uris)
  (dolist (uri uris)
    (add-to-list 'apt-deb822-sources-mode-font-lock-keywords
                 `(,(concat uri "://[^ \t\n]+$") . font-lock-doc-face))))

(defun apt-deb822-sources-mode--font-lock-add-field-names (field-names)
  (dolist (field-name field-names)
    (add-to-list 'apt-deb822-sources-mode-font-lock-keywords
                 `(,(concat "^" field-name ":") . font-lock-keyword-face))))

(defun apt-deb822-sources-mode--font-lock-add-types (types)
  (dolist (type types)
    (add-to-list 'apt-deb822-sources-mode-font-lock-keywords
                 `(,(concat "^Types:.*\\_<\\(" type "\\)\\_>")
                   (1 font-lock-function-name-face)))))

(defun apt-deb822-sources-mode--font-lock-add-suites (suites)
  (dolist (suite suites)
    ;; FIXME: Ideally this should be done using a single regexp, but it seems
    ;; that the font lock regexp detection stops at the shortest match.
    (dolist (actual-suite `(,suite
                            ,(concat suite "-security")
                            ,(concat suite "-updates")
                            ,(concat suite "-backports")))
      (add-to-list 'apt-deb822-sources-mode-font-lock-keywords
                 `(,(concat "^Suites:.*\\_<\\(" actual-suite "\\)\\_>")
                   (1 font-lock-type-face))))))

(defun apt-deb822-sources-mode--font-lock-add-components (components)
  (dolist (component components)
    (add-to-list 'apt-deb822-sources-mode-font-lock-keywords
                 `(,(concat "^Components:.*\\_<\\(" component "\\)\\_>")
                   (1 font-lock-variable-name-face)))))

;;;###autoload
(define-derived-mode apt-deb822-sources-mode fundamental-mode
  "apt-deb822-sources"
  (use-local-map apt-deb822-sources-mode-map)
  (set-syntax-table apt-deb822-sources-mode-syntax-table)
  ;; Add font locks
  (apt-deb822-sources-mode--font-lock-add-enabled
   apt-deb822-sources-mode--enabled)
  (apt-deb822-sources-mode--font-lock-add-field-names
   apt-deb822-sources-mode--field-names)
  (apt-deb822-sources-mode--font-lock-add-types apt-deb822-sources-mode--types)
  (apt-deb822-sources-mode--font-lock-add-suites
   apt-deb822-sources-mode--suites)
  (apt-deb822-sources-mode--font-lock-add-components
   apt-deb822-sources-mode--components)
  (apt-deb822-sources-mode--font-lock-add-uris
   apt-deb822-sources-mode--uris)
  ;; The comment font lock should be the last to ensure it takes precedence.
  (apt-deb822-sources-mode--font-lock-add-comments)
  (setq-local font-lock-defaults
              '(apt-deb822-sources-mode-font-lock-keywords
                nil  ;; keywords-only
                nil  ;; case-fold
                nil  ;; syntax-alist
                )))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("sources\\.list\\.d/.*\\.sources\\'" . apt-deb822-sources-mode))

(provide 'apt-deb822-sources)

;;; apt-deb822-sources.el ends here
