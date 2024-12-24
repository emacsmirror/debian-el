(require 'apt-sources)
(require 'ert)
(require 'faceup)

(defvar apt-sources-font-lock-test-dir (faceup-this-file-directory))

(defun apt-sources-font-lock-test-apps (file)
  "Test FILE is fontified as the corresponding .facup file describes."
  (faceup-test-font-lock-file 'apt-sources-mode
                              (concat apt-sources-font-lock-test-dir file)))
(faceup-defexplainer apt-sources-font-lock-test-apps)

(ert-deftest apt-sources-font-test ()
  (should (apt-sources-font-lock-test-apps
           "faceup/apt-sources/basic.list")))
