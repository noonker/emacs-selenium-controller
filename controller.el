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

;; Init

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
;; Change find element into find elements
;; Better recording

;;; Code:


(defgroup controller-mode nil
  "controller-mode variable group"
  :prefix "controller-"
  :group 'applications
  )

(defvar controller-browser "firefox"
  "default browser to be used for webdriver"
  )

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
  (setq controller-is-recording nil)
  (run-python)
  (controller-init)
  (python-shell-send-file "/home/user/git/emacs-selenium-controller/controller.py")
  (controller-resize-browser)
  (controller-mode)
  )

(define-derived-mode controller-mode special-mode "controller-mode"
  (define-key controller-mode-map (kbd "f") 'controller-find)
  (define-key controller-mode-map (kbd "j") 'controller-scroll-down)
  (define-key controller-mode-map (kbd "k") 'controller-scroll-up)
  (define-key controller-mode-map (kbd "h") 'controller-scroll-left)
  (define-key controller-mode-map (kbd "l") 'controller-scroll-right)
  (define-key controller-mode-map (kbd "p") 'controller-page-up)
  (define-key controller-mode-map (kbd "n") 'controller-page-down)
  (define-key controller-mode-map (kbd "TAB") 'controller-send-tab)
  (define-key controller-mode-map (kbd "i") 'controller-input-mode)
  (define-key controller-mode-map (kbd "e") 'controller-refresh)
  (define-key controller-mode-map (kbd "z") 'controller-send-escape)
  (define-key controller-mode-map (kbd "w") 'controller-close-tab)
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
  (define-key controller-mode-map (kbd "m") 'controller-print-highlighted)
  (define-key controller-mode-map (kbd "T") 'controller-new-tab)
  )

;; Unexposed
(defun controller-init ()
  "Initialize controller"
  (send-to-python "from selenium import webdriver")
  (send-to-python "from selenium.webdriver.common.by import By")
  (let (( browser-command "controller = webdriver.Firefox()" )) 
    (cond ((string= controller-browser "firefox") (setq browser-command "controller = webdriver.Firefox()"))
	  ((string= controller-browser "safari") (setq browser-command "controller = webdriver.Safari()"))
	  ((string= controller-browser "chrome") (setq browser-command "controller = webdriver.Chrome()")))
    (if controller-is-recording
	(setq controller-recording (cons browser-command controller-recording))
      (send-to-python browser-command)
	)
    )
  (send-to-python "from selenium.webdriver.common.action_chains import ActionChains")
  (send-to-python "from selenium.webdriver.common.keys import Keys")
  
  )

(defun send-to-python ( command &optional arg bypass)
  "Main function to route commands to python"
  (if arg (setq command (format command arg)))
  (if (and controller-is-recording (not bypass))
      (setq controller-recording (cons command controller-recording)))
  (remove "" (split-string (python-shell-send-string-no-output (format "%s" command)) "\n"))
  )

(defun controller-marker-focus (marker)
  "click on the thing"
  (python-shell-send-string-no-output (format "element = select_marker(\"%s\", markers)" marker))
  )

(defun controller-switch-tab-switch (marker)
  "Switch to tab"
  (send-to-python "switch_window_switch(\"%s\", windows)" marker))

(defun controller-attribute-chooser ()
  "Choose an attribute for the element."	
  (let ((id (car (send-to-python "element.get_attribute(\"id\")" nil t))) ;; id
	(class (car (send-to-python "element.get_attribute(\"class\")" nil t))) ;; class name
	;; TODO css selector
	(name (car (send-to-python "element.get_attribute(\"name\")" nil t))) ;; name
	;; TODO xpath
	(tag (car (send-to-python "element.tag_name" nil t))) ;; tag name
	(link (car (send-to-python "element.get_attribute(\"href\")" nil t))));; link text
    (setq options `((,(format "id - %s" id) . ,(format "element = controller.find_element_by_id(%s)" id))
		    (,(format "class - %s" class) . ,(format "element = controller.find_element_by_class(%s)" class))
		    (,(format "name - %s" name) . ,(format "element = controller.find_element_by_name(%s)" name))
		    (,(format "tag - %s" tag) . ,(format "element = controller.find_element_by_tag_name(%s)" tag))
		    (,(format "link - %s" link) . ,(format "element =controller.find_element_by_link_text(%s)" link))
		    ))
    ;; TODO partial link text
    (setq helm-attribute-chooser
	  `((name . "Choose a selector to add to the recording")
            (candidates . ,(mapcar 'car options))
            (action . (lambda (candidate)
			(setq controller-recording (cons (cdr (assoc candidate options)) controller-recording))))))
    (helm :sources '(helm-attribute-chooser))
    )
  )

;; Exposed

(defun controller-page-down ()
  "Scroll Down."
  (interactive)
  (send-to-python "ActionChains(controller).key_down(Keys.PAGE_DOWN).perform()")
  )

(defun controller-page-up ()
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

(defun controller-scroll-up ()
  "Scroll Left."
  (interactive)
  (send-to-python "ActionChains(controller).key_down(Keys.LEFT).perform()")
  )

(defun controller-scroll-down ()
  "Scroll Right."
  (interactive)
  (send-to-python "ActionChains(controller).key_down(Keys.RIGHT).perform()")
  )

(defun controller-refresh ()
  "Scroll Down."
  (interactive)
  (send-to-python "controller.refresh()")
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

(defun controller-new-tab ()
  "Open new tab"
  (interactive)
  (send-to-python "controller.execute_script(\"window.open('https://google.com');\")")
  )

(defun controller-send-enter ()
  "Send Enter."
  (interactive)
  (send-to-python "ActionChains(controller).send_keys(Keys.ENTER).perform()")
  )

(defun controller-close-tab ()
  "Send Enter."
  (interactive)
  (send-to-python "controller.close()")
  )

(defun controller-send-escape ()
  "Send Escape."
  (interactive)
  (send-to-python "ActionChains(controller).send_keys(Keys.ESCAPE).perform()")
  )

(defun controller-send-tab ()
  "Send Escape."
  (interactive)
  (send-to-python "ActionChains(controller).send_keys(Keys.TAB).perform()")
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
  (setq options (split-string (python-shell-send-string-no-output "print(temp.text)") "\n"))
  (setq some-helm-source
	'((name . "Guided Search")
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

(defun controller-print-highlighted ()
  "Print the text of this element."
  (interactive)
  (send-to-python "print(element.text)")
  )

(defun controller-bookmark-page ()
  "Bookmark the current page"
  (interactive)
  (setq controller-bookmarks (cons (python-shell-send-string-no-output "controller.current_url") controller-bookmarks))
  )

(defun controller-highlight ()
  "Highlight Found Element."
  (interactive)
  (send-to-python "highlight(element)" nil t))

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
  (python-shell-send-string-no-output "markers = create_markers(controller)")
  (setq options (split-string (python-shell-send-string-no-output "print(\" \".join(markers.keys()))")))
  (setq some-helm-source
	'((name . "Choose Element")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-marker-focus candidate)))))
  (progn
    (helm :sources '(some-helm-source))
    (if controller-is-recording
	(controller-attribute-chooser)))
  (controller-quit-find)
  (controller-highlight)
  )

(defun controller-quit-find ()
  "Quit find mode"
  (interactive)
  (python-shell-send-string "close_markers(controller)")
  )

(defun controller-switch-tab ()
  "switch tabs"
  (interactive)
  (setq options (send-to-python "windows = switch_window(controller)"))
  (setq some-helm-source
	'((name . "Choose Tab")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-switch-tab-switch candidate)))))
  (helm :sources '(some-helm-source))
  )

(defun controller-record ()
  "Toggle recording."
  (interactive)
  (if controller-is-recording
      (progn
	(with-current-buffer  (get-buffer-create "*controller-output*")
	  (mapcar (lambda (line) (insert (format "%s\n" line))) (reverse controller-recording)))
	(setq controller-is-recording nil)
	(message "Stopped Recording!")
	)
    (progn
      (setq controller-recording '())
      (setq controller-is-recording t)
      (controller-init)
      (message "Recording!")
      )
    )
  )

(provide 'controller)

;;; controller.el ends here
