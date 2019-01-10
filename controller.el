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

;;; TODOS

;;; Essential
;; TODO Grabber
;;; Choose elements on a page to save to a list to be used in Emacs
;; Bring up a helm window to choose the selector (ID, class, XPATH, etc )
;; 
;;; For non repeatable commands if recording mode is active bring up the chooser 
;; Better recording w/ output
;; Send Escape
;; Kill Tab
;; Reopen last tab
;; Reload

;;; Non-Essential
;; Add constraints to make sure variables exist and return errors if they do not
;; Default https:// prefix for URL entry
;; Better bookmarks
;; Figure out better find options.  Currently find mode is pretty finnicky.
;; Enable support for browser addons
;; Support pass https://github.com/NicolasPetton/pass mode for scripts
;; Auto resize windows to something that makes sense
;; Mark multiple css selectors when choosing an attribute
;; Embed more python in this

;;; Code:


(defgroup controller-mode nil
  "controller-mode variable group"
  :prefix "controller-"
  :group 'applications
  )

(defvar controller-browser "firefox"
  "default browser to be used for webdriver")

(defcustom controller-history '()
  "history of URLs navigated to"
  :group 'controller-mode
  :type 'listp)

(defcustom controller-bookmarks '()
  "bookmarked websites"
  :group 'controller-mode
  :type 'listp)


(defun browser-controller ()
  "Browser Controller."
  (interactive)
  (switch-to-buffer "browser-controller")
  (buffer-disable-undo "browser-controller")
  (run-python)
  (python-shell-send-string "from selenium import webdriver")
  (python-shell-send-string "from selenium.webdriver.common.by import By")
  (cond ((string= controller-browser "firefox") (python-shell-send-string "controller = webdriver.Firefox()"))
	((string= controller-browser "safari") (python-shell-send-string "controller = webdriver.Safari()"))
	((string= controller-browser "chrome") (python-shell-send-string "controller = webdriver.Chrome()"))
	)
  (python-shell-send-string "from selenium.webdriver.common.action_chains import ActionChains")
  (python-shell-send-string "from selenium.webdriver.common.keys import Keys")
  (python-shell-send-file "controller.py")
  (setq controller-is-recording nil)
  (setq controller-recording '())
  (controller-resize-browser)
  (controller-mode)
  )

(define-derived-mode controller-mode special-mode "controller-mode"
  (define-key controller-mode-map (kbd "f") 'controller-find)
  (define-key controller-mode-map (kbd "j") 'controller-scroll-down)
  (define-key controller-mode-map (kbd "k") 'controller-scroll-up)
  (define-key controller-mode-map (kbd "h") 'controller-scroll-left)
  (define-key controller-mode-map (kbd "l") 'controller-scroll-right)
  (define-key controller-mode-map (kbd "i") 'controller-input-mode)
  (define-key controller-mode-map (kbd "<return>") 'controller-send-enter)
  (define-key controller-mode-map (kbd "d") 'controller-quit-find)
  (define-key controller-mode-map (kbd "L") 'controller-url-goto)
  (define-key controller-mode-map (kbd "e") 'controller-forward-history)
  (define-key controller-mode-map (kbd "a") 'controller-backward-history)
  (define-key controller-mode-map (kbd "s") 'controller-search-in-page)
  (define-key controller-mode-map (kbd "S") 'controller-guided-search-in-page)
  (define-key controller-mode-map (kbd "r") 'controller-record)
  (define-key controller-mode-map (kbd "c") 'controller-click-highlighted)
  (define-key controller-mode-map (kbd "*") 'controller-highlight)
  (define-key controller-mode-map (kbd "b") 'controller-bookmark-page)
  (define-key controller-mode-map (kbd "t") 'controller-switch-tab)
  )

;; Unexposed

(defun send-to-python ( command &optional arg )
  "Main function to route commands to python"
  (if arg (setq command (format command arg)))
  (if controller-is-recording
      (setq controller-recording (cons command controller-recording)))
  (python-shell-send-string-no-output (format "%s" command))
  )

(defun controller-find-click (marker)
  "click on the thing"
  (send-to-python "click_marker(\"%s\", markers)" marker)
  )

(defun controller-switch-tab-switch (marker)
  "Switch to tab"
  (send-to-python "switch_window_switch(\"%s\", windows)" marker))

(defun controller-attribute-chooser ()
  "Choose an attribute for the element."
  (let ((id (python-shell-send-string-no-output "element.get_attribute(\"id\")")) ;; id
	(class (python-shell-send-string-no-output "element.get_attribute(\"class\")")) ;; class name
	;; TODO css selector
	(name (python-shell-send-string-no-output "element.get_attribute(\"name\")")) ;; name
	;; TODO xpath
	(tag (python-shell-send-string-no-output "element.tag_name")) ;; tag name
	(link (python-shell-send-string-no-output "element.get_attribute(\"href\")")));; link text
    ;; TODO partial link text
    (setq helm-attribute-chooser
	  '((name . "HELM at the Emacs")
            (candidates . options)
            (action . (lambda (candidate)
			(controller-switch-tab-switch candidate)))))
    (helm :sources '(helm-attribute-chooser))
    )
  )

;; Exposed

(defun controller-scroll-down ()
  "Scroll Down."
  (interactive)
  (send-to-python "ActionChains(controller).key_down(Keys.PAGE_DOWN).perform()")
  )

(defun controller-scroll-up ()
  "Scroll Up."
  (interactive)
  (send-to-python "ActionChains(controller).key_down(Keys.PAGE_UP).perform()")
  )

(defun controller-scroll-left ()
  "Scroll Left."
  (interactive)
  (send-to-python "ActionChains(controller).key_down(Keys.LEFT).perform()")
  )

(defun controller-scroll-right ()
  "Scroll Right."
  (interactive)
  (send-to-python "ActionChains(controller).key_down(Keys.RIGHT).perform()")
  )

(defun controller-input-mode (input)
  "Start Input Mode"
  (interactive "sInput: ")
  (send-to-python "ActionChains(controller).send_keys(\"%s\").perform()" input)
  )

(defun controller-url-goto (input)
  "Start Input Mode"
  (interactive "sURL: ")
  (send-to-python "controller.get(\"%s\")" input)
  (setq controller-history (cons input controller-history))
  )

(defun controller-send-enter ()
  "Send Enter."
  (interactive)
  (send-to-python "ActionChains(controller).send_keys(Keys.ENTER).perform()")
  )

(defun controller-backward-history ()
  "Go Back."
  (interactive)
  (send-to-python "controller.back()"))

(defun controller-forward-history ()
  "Go Forward."
  (interactive)
  (send-to-python "controller.forward()"))

(defun controller-search-in-page (input)
  "Search for element"
  (interactive "sText: ")
  (send-to-python "element = controller.find_element_by_xpath(\"//*[contains(text(), '%s')]\")" input)
  (controller-highlight)
  )

(defun controller-guided-search-in-page ()
  "Like search but with helm"
  (interactive)
  (python-shell-send-string "temp = controller.find_elements(By.TAG_NAME, \"body\")")
  (python-shell-send-string "temp = temp[0]")
  (setq options (split-string (python-shell-send-string-no-output "print(temp.text)")))
  (setq some-helm-source
	'((name . "HELM at the Emacs")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-search-in-page candidate)))))
  (helm :sources '(some-helm-source))
  )


(defun controller-click-highlighted ()
  "Click highlighted Element."
  (interactive)
  (send-to-python "element.click()")
  )

(defun controller-bookmark-page ()
  "Bookmark the current page"
  (interactive)
  (setq controller-bookmarks (cons (python-shell-send-string-no-output "controller.current_url") controller-bookmarks))
  )

(defun controller-highlight ()
  "Highlight Found Element."
  (interactive)
  (send-to-python "highlight(element)"))

(defun controller-resize-browser ()
  "Resize the browser window."
  (interactive)
  (send-to-python "controller.maximize_window()")
  (send-to-python "resolution = controller.get_window_size()")
  (send-to-python "controller.set_window_size(resolution['width'], resolution['height'] * .8)")
  )

(defun controller-find ()
  "Highlight elements on the page and open a helm window for selection"
  (interactive)
  (send-to-python "markers = create_markers(controller)")
  (setq options (split-string (send-to-python "print(\" \".join(markers.keys()))")))
  (setq some-helm-source
	'((name . "HELM at the Emacs")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-find-click candidate)))))
  (helm :sources '(some-helm-source))
  (controller-quit-find)
  )

(defun controller-quit-find ()
  "Quit find mode"
  (interactive)
  (send-to-python "close_markers(controller)")
  )

(defun controller-switch-tab ()
  "switch tabs"
  (interactive)
  (setq options (split-string (send-to-python "windows = switch_window(controller)") "\n"))
  (setq some-helm-source
	'((name . "HELM at the Emacs")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-switch-tab-switch candidate)))))
  (helm :sources '(some-helm-source))
  )

(provide 'controller)

;;; controller.el ends here
