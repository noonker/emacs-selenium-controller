;;; brower-controller.el --- control a web browser with Emacs & Selenium.

;;; Commentary:

;;; TODOS

;; TODO Implement Find Mode for Forms
;; TODO Find mode for buttons
;; TODO Open in tab open in new tab
;; TODO Exclusive buffer
;; TODO BEtter variable names
;; TODO Implement Record Mode
;; TODO browser history
;; TODO Quit and Cleanup
;; TODO Comments
;; TODO Error handling
;; TODO Grabber
;;; Choose elements on a page to save to a list to be used in Emacs
;;; Bring up a helm window to choose the selector (ID, class, XPATH, etc )
;; TODO Inline Python
;; TODO Browser Options
;; TODO Switch Tabs
;; TODO Bookmarks && Bookmarked Scripts
;; TODO Helm Search with Text Candidates
;;; In [759]: c = a.find_elements(By.TAG_NAME, "body")
;;; In [760]: c = c[0]
;;; In [761]: c.text

;;; Code:

(defun browser-controller ()
  "Browser Controller."
  (interactive)
  (switch-to-buffer "browser-controller")
  (buffer-disable-undo "browser-controller")
  (run-python)
  (python-shell-send-string "from selenium import webdriver")
  (python-shell-send-string "a = webdriver.Firefox()")
  (python-shell-send-string "from selenium.webdriver.common.action_chains import ActionChains")
  (python-shell-send-string "from selenium.webdriver.common.keys import Keys")
  (python-shell-send-file "highlighter.py")
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
  (python-shell-send-string "ActionChains(a).key_down(Keys.PAGE_DOWN).perform()")
  )

(defun controller-scroll-up ()
  "Scroll Up."
  (interactive)
  (python-shell-send-string "ActionChains(a).key_down(Keys.PAGE_UP).perform()")
  )

(defun controller-scroll-left ()
  "Scroll Left."
  (interactive)
  (python-shell-send-string "ActionChains(a).key_down(Keys.LEFT).perform()")
  )

(defun controller-scroll-right ()
  "Scroll Right."
  (interactive)
  (python-shell-send-string "ActionChains(a).key_down(Keys.RIGHT).perform()")
  )

(defun controller-input-mode (input)
  "Start Input Mode"
  (interactive "sInput: ")
  (python-shell-send-string (format "ActionChains(a).send_keys(\"%s\").perform()" input))
  )

(defun controller-url-goto (input)
  "Start Input Mode"
  (interactive "sURL: ")
  (python-shell-send-string (format "a.get(\"%s\")" input))
  )

(defun controller-send-enter ()
  "Send Enter."
  (interactive)
  (python-shell-send-string "ActionChains(a).send_keys(Keys.ENTER).perform()")
  )

(defun controller-backward-history ()
  "Go Back."
  (interactive)
  (python-shell-send-string "a.back()"))

(defun controller-forward-history ()
  "Go Forward."
  (interactive)
  (python-shell-send-string "a.forward()"))

(defun controller-search-in-page (input)
  "Search for element"
  (interactive "sText: ")
  (python-shell-send-string (format "c = a.find_element_by_xpath(\"//*[contains(text(), '%s')]\")" input))
  (controller-highlight)
  )

(defun controller-click-highlighted ()
  "Click highlighted Element."
  (interactive)
  (python-shell-send-string "c.click()")
  )

(defun controller-highlight ()
  "Highlight Found Element."
  (interactive)
  (python-shell-send-string "highlight(c)"))

(run-python)
(python-shell-send-string "from selenium import webdriver")
(python-shell-send-string "a = webdriver.Firefox()")


(defun controller-resize-browser ()
  "Click highlighted Element."
  (interactive)
  (python-shell-send-string "a.maximize_window()")
  (python-shell-send-string "resolution = a.get_window_size()")
  (python-shell-send-string "a.set_window_size(resolution['width'], resolution['height'] * .8)")
  )
