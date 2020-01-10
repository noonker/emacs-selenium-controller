;;; brower-controller.el --- control a web browser with Emacs & Selenium.

;; Author: "Joshua Person" <noonker@pm.me>
;; Time-stamp: <09 January 2019>
;; Version: 0.1

;;; Commentary:

;;; Nomenclature:
;; controller - the webdriver object
;; tags - the css popup text to facilitate identification in find modes
;; markers - the css popovers
;; element - representing an obbject on the page
;; window - a tab or window

;;; Code:

;; Imports
(require 'python)
(require 'org)
;; (require 'controller)

;;; Constants
(defconst directory-of-python-file (file-name-directory load-file-name))
(defvar controller-python-file (expand-file-name "python.org" directory-of-python-file))

;;; Functions
(defun controller-python-get (command &optional arg)
   "This is the main entrypoint for getting code in the Python abstraction.
    Given some COMMAND name the actual function is loaded from it's org source
    block in python.org.
    Pass an optional ARG to have the code block automatically formatted."
   (let ((contents (with-temp-buffer
		     (insert-file-contents controller-python-file)
		     (org-babel-goto-named-src-block command)
		     (org-element-property :value (org-element-at-point)))))
     (if arg (format contents arg) contents)))

(defun controller-execute-python (command &optional arg bypass)
  "This is the main entrypoint for executing code in the Python abstraction.
    Given some COMMAND name the actual function is loaded from it's org source
    block in python.org and run.
    Pass an optional ARG to have the code block automatically formatted.
    Passing BYPASS means it will not be added to the recording list if in
    recording mode."
  (let* ((unformatted_command (controller-python-get command))
	 (command             (if arg
				  (format unformatted_command arg)
				unformatted_command)))
    (if (and controller-is-recording (not bypass))
	(setq controller-recording (cons command controller-recording)))
    (remove "" (split-string (python-shell-send-string-no-output command (get-process "browser-controller")) "\n"))))

;;; Provide
(provide 'controller-python)

;;; controller-python.el ends here
