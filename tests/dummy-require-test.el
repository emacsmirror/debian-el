(require 'apt-sources)
(require 'apt-utils)
(require 'debian-bug)
(require 'debian-el)
(require 'deb-view)
(require 'gnus-BTS)
(require 'preseed)

(ert-deftest dummy-require-test ()
  "A dummy test to ensure all add-ons are byte-compiled.

This helps catch bugs and dependency issues when developing
without real unit tests.  When real tests are available please
feel free to remove this one."
  t)
