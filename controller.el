;;; brower-controller.el --- control a web browser with Emacs & Selenium.

;;; Commentary:

;;; Nomenclature:
;; controller - the webdriver object
;; tags - the css popup text to facilitate identification in find modes
;; markers - the css popovers
;; element - representing an obbject on the page
;; window - a tab or window

;;; TODOS

;;; Essential
;; TODO Implement Record Mode
;; TODO Comments && Registering all commands
;; TODO Grabber
;;; Choose elements on a page to save to a list to be used in Emacs
;;; Bring up a helm window to choose the selector (ID, class, XPATH, etc )
;; TODO Helm Search with Text Candidates
;;; In [759]: c = a.find_elements(By.TAG_NAME, "body")
;;; In [760]: c = c[0]
;;; In [761]: c.text
;; TODO Initialize defaults

;;; Non-Essential
;; Default http
;; Better bookmarks
;; Figure out better find options
;; Enable Addons
;; Password Manager for scripts
;; Auto resize windows

;;; Code:


(defgroup controller-mode nil
  "controller-mode variable group"
  :prefix "controller-"
  :group 'applications
  )

(defvar controller-browser "firefox2" "default browser to be used for webdriver")

(defcustom controller-history
  "history of URLs navigated to"
  :group 'controller-mode
  :type 'listp)

(defcustom controller-bookmarks
  "bookmarked websites"
  :group 'controller-mode
  :type 'listp)

;; TODO initialize defaults
(setq controller-browsxer "firefox")
(setq controller-history '())
(setq controller-bookmarks '())

(defun browser-controller ()
  "Browser Controller."
  (interactive)
  (switch-to-buffer "browser-controller")
  (buffer-disable-undo "browser-controller")
  (run-python)
  (python-shell-send-string "from selenium import webdriver")
  (cond ((string= controller-browser "firefox") (python-shell-send-string "controller = webdriver.Firefox()"))
	((string= controller-browser "safari") (python-shell-send-string "controller = webdriver.Safari()"))
	((string= controller-browser "chrome") (python-shell-send-string "controller = webdriver.Chrome()"))
	)
  (python-shell-send-string "from selenium.webdriver.common.action_chains import ActionChains")
  (python-shell-send-string "from selenium.webdriver.common.keys import Keys")
  (python-shell-send-file "hightlighter.py")
  (controller-mode)
  (controller-resize-browser)
  )

(define-derived-mode controller-mode special-mode "controller-mode"
  (define-key controller-mode-map (kbd "f") 'controller-find)
  (define-key controller-mode-map (kbd "j") 'controller-scroll-down)
  (define-key controller-mode-map (kbd "k") 'controller-scroll-up)
  (define-key controller-mode-map (kbd "h") 'controller-scroll-left)
  (define-key controller-mode-map (kbd "l") 'controller-scroll-right)
  (define-key controller-mode-map (kbd "i") 'controller-input-mode)
  (define-key controller-mode-map (kbd "<return>") 'controller-send-enter)
  (define-key controller-mode-map (kbd "<esc>") 'controller-quit-find)
  (define-key controller-mode-map (kbd "L") 'controller-url-goto)
  (define-key controller-mode-map (kbd "e") 'controller-forward-history)
  (define-key controller-mode-map (kbd "a") 'controller-backward-history)
  (define-key controller-mode-map (kbd "s") 'controller-search-in-page)
  (define-key controller-mode-map (kbd "r") 'controller-record)
  (define-key controller-mode-map (kbd "c") 'controller-click-highlighted)
  (define-key controller-mode-map (kbd "*") 'controller-highlight)
  )

(defun controller-scroll-down ()
  "Scroll Down."
  (interactive)
  (python-shell-send-string "ActionChains(controller).key_down(Keys.PAGE_DOWN).perform()")
  )

(defun controller-scroll-up ()
  "Scroll Up."
  (interactive)
  (python-shell-send-string "ActionChains(controller).key_down(Keys.PAGE_UP).perform()")
  )

(defun controller-scroll-left ()
  "Scroll Left."
  (interactive)
  (python-shell-send-string "ActionChains(controller).key_down(Keys.LEFT).perform()")
  )

(defun controller-scroll-right ()
  "Scroll Right."
  (interactive)
  (python-shell-send-string "ActionChains(controller).key_down(Keys.RIGHT).perform()")
  )

(defun controller-input-mode (input)
  "Start Input Mode"
  (interactive "sInput: ")
  (python-shell-send-string (format "ActionChains(controller).send_keys(\"%s\").perform()" input))
  )

(defun controller-url-goto (input)
  "Start Input Mode"
  (interactive "sURL: ")
  (python-shell-send-string (format "controller.get(\"%s\")" input))
  (setq controller-history (cons input controller-history))
  )

(defun controller-send-enter ()
  "Send Enter."
  (interactive)
  (python-shell-send-string "ActionChains(controller).send_keys(Keys.ENTER).perform()")
  )

(defun controller-backward-history ()
  "Go Back."
  (interactive)
  (python-shell-send-string "controller.back()"))

(defun controller-forward-history ()
  "Go Forward."
  (interactive)
  (python-shell-send-string "controller.forward()"))

(defun controller-search-in-page (input)
  "Search for element"
  (interactive "sText: ")
  (python-shell-send-string (format "c = controller.find_element_by_xpath(\"//*[contains(text(), '%s')]\")" input))
  (controller-highlight)
  )

(defun controller-click-highlighted ()
  "Click highlighted Element."
  (interactive)
  (python-shell-send-string "c.click()")
  )

(defun controller-bookmark-page ()
  "Bookmark the current page"
  (interactive)
  (setq controller-bookmarks (cons (python-shell-send-string-no-output "controller.current_url") controller-bookmarks))
  )

(defun controller-highlight ()
  "Highlight Found Element."
  (interactive)
  (python-shell-send-string "highlight(c)"))

(defun controller-resize-browser ()
  "Click highlighted Element."
  (interactive)
  (python-shell-send-string "controller.maximize_window()")
  (python-shell-send-string "resolution = controller.get_window_size()")
  (python-shell-send-string "controller.set_window_size(resolution['width'], resolution['height'] * .8)")
  )

(defun controller-find ()
  "Highlight elements on the page and open a helm window for selection"
  (interactive)
  (python-shell-send-string-no-output "markers = create_markers(controller)")
  (setq options (split-string (python-shell-send-string-no-output "print(\" \".join(markers.keys()))")))
  (setq some-helm-source
	'((name . "HELM at the Emacs")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-find-click candidate)))))
  (helm :sources '(some-helm-source))
  )

(defun controller-quit-find ()
  "Quit find mode"
  (interactive)
  (python-shell-send-string "close_markers(controller)")
  )

(defun controller-find-click (marker)
  "click on the thing"
  (python-shell-send-string (format "click_marker(\"%s\", markers)" marker))
  )

(defun controller-switch-tab-switch (marker)
  "Switch to tab"
  (python-shell-send-string (format "switch_window_switch(\"%s\", windows)" marker)))

(defun controller-switch-tab ()
  "switch tabs"
  (interactive)
  (setq options (split-string (python-shell-send-string-no-output "windows = switch_window(controller)") "\n"))
  (setq some-helm-source
	'((name . "HELM at the Emacs")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-switch-tab-switch candidate)))))
  (helm :sources '(some-helm-source))
  )

(provide 'controller)

;;; controller.el ends here
