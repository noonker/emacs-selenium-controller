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
;;(require 'controller)
(require 'json)

(defvar controller-parsed-name)
(defvar controller-parsed-json)

(define-minor-mode controller-cookie-edit-mode
  "Toggle controller-cookie-edit mode."
  :lighter " Cookies!"
  :keymap (let
	      ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c s") 'controller-cookie-buffer-update)
	    map))

(defun controller-cookie-buffer-update ()
  "Update the selenium instance with the cookie modified in the browser."
  (interactive)
  (let  ((json-buffer (replace-regexp-in-string "\n" ""
			(buffer-substring-no-properties (point-min) (point-max)))))
    (controller-execute "delete_cookie" controller-parsed-name)
    (controller-execute "add_cookie" json-buffer)
    (message "Cookie Updated!")))

(defun controller-list-cookies ()
  "List the cookies."
  (interactive)
  (let* ((cookie      (helm :sources (helm-build-sync-source "Select Cookie"
				       :candidates (mapcar (lambda (x) (format "%s" x))
							   (controller-execute "get_cookies"))
				       :fuzzy-match t)
			    :buffer "*helm cookies*"))
	 (buffer      (get-buffer-create  "*cookie-edit-source*"))
	 (parsed-json (json-read-from-string cookie))
	 (parsed-name (cdr (assoc 'name parsed-json)))
	)
    (with-output-to-temp-buffer buffer
      (switch-to-buffer-other-window buffer)
      (erase-buffer)
      (insert cookie)
      (json-reformat-region (point-min) (point-max))
      (json-mode)
      (setq-local controller-parsed-json parsed-json)
      (setq-local controller-parsed-name parsed-name))))

(provide 'controller-cookies)
;;; controller-cookies.el ends here
