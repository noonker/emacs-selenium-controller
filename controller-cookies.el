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
(require 'controller)
(require 'json)

(defun controller-list-cookies ()
  "List the cookies."
  (interactive)
  (let ((cookie (helm :sources (helm-build-sync-source "Select Cookie"
				 :candidates (mapcar (lambda (x) (format "%s" x)) (json-read-from-string (substring (car (controller-execute "get_cookies")) 1 -1)))
				 :fuzzy-match t)
		      :buffer "*helm cookies*"))
	(buffer (get-buffer-create  "*cookie-edit-source*"))
	)
    (with-output-to-temp-buffer buffer
      (switch-to-buffer-other-window buffer)
      (erase-buffer)
      (insert cookie)
      
      (emacs-lisp-mode)
      (read-only-mode)
     )))

(provide 'controller-cookies)
;;; controller-cookies.el ends here
