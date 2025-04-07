(require 'apt-deb822-sources)
(require 'ert)
(require 'faceup)

(defvar apt-deb822-sources-font-lock-test-dir (faceup-this-file-directory))

(defun apt-deb822-sources-font-lock-test-apps (file)
  "Test FILE is fontified as the corresponding .facup file describes."
  (faceup-test-font-lock-file 'apt-deb822-sources-mode
                              (concat apt-deb822-sources-font-lock-test-dir
                                      file)))
(faceup-defexplainer apt-deb822-sources-font-lock-test-apps)

(ert-deftest apt-deb822-sources-font-test ()
  (should (apt-deb822-sources-font-lock-test-apps
           "faceup/apt-deb822-sources/basic.sources"))
  (should (apt-deb822-sources-font-lock-test-apps
           "faceup/apt-deb822-sources/more-suites.sources"))
  (should (apt-deb822-sources-font-lock-test-apps
           "faceup/apt-deb822-sources/typos.sources")))
