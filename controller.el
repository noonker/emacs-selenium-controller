;;; brower-controller.el --- control a web browser with Emacs & Selenium.

;; Author: "Joshua Person" <noonker@pm.me>
;; Time-stamp: <09 January 2019>
;; Version: 0.2

;;; Commentary:

;;; Nomenclature:
;; controller - the webdriver object
;; tags - the css popup text to facilitate identification in find modes
;; markers - the css popovers
;; element - representing an obbject on the page
;; window - a tab or window

;;; TODOS

;;; Essential
;; Allow saving of history
;; Script browser and runner
;; Implement persistence mechanisms.  Currently all the things are lost on restart
;; Better bookmarks
;; Better importing (load-file)

;;; Non-Essential
;; Allow controlling multiple browsers
;; - This will likely mean putting all controllers in a list
;; - Maybe have them be named and allow specification of which browser gets which action
;; - ..and doing something like a foreach on each of the browser objects
;; More interactive browser controller window
;; - Right now this is just a black box.  What I'd ideally like this to do is display some browser
;; - ..information or be a small window IN the modeline
;; Support browsermob proxy
;; Support selenium profiles
;; Figure out the structure of actual Emacs packages. (I don't think it's requre AND load-file)
;; Add constraints to make sure variables exist and return errors if they do not
;; Figure out better find options.  Currently find mode is pretty finnicky.
;; Enable support for browser addons
;; Support pass https://github.com/NicolasPetton/pass mode for scripts
;; Auto resize windows to something that makes sense
;; Mark multiple css selectors when choosing an attribute
;; User Agent Switching
;; Header Modification
;; Change find element into find elements
;; Special Keys - Helm chooser
;; Undo
;; Pseudocode e.c. "went to firefox.com clicked on "log in"
;; Grid Integration
;; Captcha Alert- STOP when captcha encoundered and let user do the captcha before continting
;; Figure out how to work around locking issues

;;; Code:

;;; Imports
(defconst directory-of-foo (file-name-directory load-file-name))
(load-file (expand-file-name "controller-python.el" directory-of-foo))
(load-file (expand-file-name "controller-cookies.el" directory-of-foo))
;(load-file "controller-python.el")
;(load-file "controller-cookies.el")

(require 'json)

;;; Mode-Related Code
(defgroup controller-mode nil
  "controller-mode variable group"
  :prefix "controller-"
  :group 'applications
  )

(defvar controller-browser "firefox" "Default browser to be used for webdriver.")
(defvar controller-implementation "python" "Default implentation to use.")
(defvar controller-is-recording nil)

(defcustom controller-history '()
  "History of URLs navigated to."
  :group 'controller-mode
  :type 'listp)

(defcustom controller-bookmarks '()
  "Bookmarked websites."
  :group 'controller-mode
  :type 'listp)

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
  (define-key controller-mode-map (kbd "W") 'controller-close-tab)
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
  (define-key controller-mode-map (kbd "B") 'controller-break)
  (define-key controller-mode-map (kbd "t") 'controller-switch-tab)
  (define-key controller-mode-map (kbd "m") 'controller-print-highlighted)
  (define-key controller-mode-map (kbd "T") 'controller-new-tab)
  (define-key controller-mode-map (kbd ".") 'controller-wait-for))

;; Program Entrypoint
(defun browser-controller ()
  "This is the entrypoint for the browser controller."
  (interactive)
  (python-shell-make-comint "python3" "browser-controller")
  (switch-to-buffer "browser-controller")
  (buffer-disable-undo "browser-controller")
  (setq controller-is-recording nil)
  (run-python)
  (controller-init)
  (controller-resize-browser)
  (controller-mode))

;;; Routing Functions
(defun controller-execute (command &optional arg bypass)
  "This is the main entrypoint for executing code in the Python abstraction.
  Given some COMMAND name the actual function is loaded from it's org source
  block in <controller-implementation>.org and run.
  Pass an optional ARG to have the code block automatically formatted.
  Passing BYPASS means it will not be added to the recording list if in
  recording mode."
  (if (string= controller-implementation "python")
      (controller-execute-python command arg bypass)))

(defun controller-get (command &optional arg)
  "This is the main entrypoint for getting code abstraction.
   Given some COMMAND name the actual function is loaded from it's org source
   block in <controller-implementation>.org.
   Pass an optional ARG to have the code block automatically formatted."
  (if (string= controller-implementation "python")
      (controller-python-get command arg)))

;; Unexposed Functions
(defun controller-init ()
  "Initialize a new browser window."
  (controller-execute "browser_choice")
  (controller-execute "apply_style" nil t)
  (controller-execute "highlight" nil t)
  (controller-execute "create_markers" nil t)
  (controller-execute "select_marker" nil t)
  (controller-execute "close_markers" nil t)
  (controller-execute "switch_window" nil t)
  (controller-execute "switch_window_switch" nil t)
  (controller-execute "generate_tags" nil t)
  (controller-execute "browser_choice" controller-browser)
  (controller-execute "initialize"))

(defun controller-marker-focus (marker)
  "click on the thing"
  (controller-execute "marker_focus" marker t))

(defun controller-switch-tab-switch (marker)
  "Switch to tab"
  (controller-execute "switch_window_switch"))

(defun controller-wait-chooser ()
  "Choose an attribute for a wait"
  (let ((id (car (controller-execute "get_element_id" nil t))) ;; id
	(class (car (controller-execute "get_element_class" nil t))) ;; class name
	;; TODO css selector
	(name (car (controller-execute "get_element_name" nil t))) ;; name
	;; TODO xpath
	(tag (car (controller-execute "get_element_tag" nil t))) ;; tag name
	(link (car (controller-execute "get_element_link" nil t))) ;; link text
	(options nil))
    (setq options `((,(format "id - %s" id) . ,(controller-get "wait_id_selection" id))
		    (,(format "class - %s" class) . ,(controller-get "wait_class_selection" class))
		    (,(format "name - %s" name) . ,(controller-get "wait_name_selection" name))
		    (,(format "tag - %s" tag) . ,(controller-get "wait_tag_selection" tag))
		    (,(format "link - %s" link) . ,(controller-get "wait_link_selection" link))
		    ))
    ;; TODO partial link text
    (setq helm-attribute-chooser
	  `((name . "Choose a selector to add to the recording")
            (candidates . ,(mapcar 'car options))
            (action . (lambda (candidate)
			(setq controller-recording (cons (cdr (assoc candidate options)) controller-recording))))))
    (helm :sources '(helm-attribute-chooser))))

(defun controller-attribute-chooser ()
  "Choose an attribute for the element."	
  (let ((id (car (controller-execute "get_element_id" nil t))) ;; id
	(class (car (controller-execute "get_element_class" nil t))) ;; class name
	;; TODO css selector
	(name (car (controller-execute "get_element_name" nil t))) ;; name
	;; TODO xpath
	(tag (car (controller-execute "get_element_tag" nil t))) ;; tag name
	(link (car (controller-execute "get_element_link" nil t))) ;; link text
	(text (car (controller-execute "get_element_text" nil t))) ;; tag text
	(options nil)
	);; link text
    (setq options `((,(format "id - %s" id) . ,(controller-get "find_id_selection" id))
		    (,(format "class - %s" class) . ,(controller-get "find_class_selection" class))
		    (,(format "name - %s" name) . ,(controller-get "find_name_selection" name))
		    (,(format "tag - %s" tag) . ,(controller-get "find_tag_selection" tag))
		    (,(format "text - %s" text) . ,(controller-get "find_text_selection" text))
		    (,(format "link - %s" link) . ,(controller-get "find_link_selection" link))
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

(defun controller-break ()
  "Break program for things like captcha"
  (interactive)
  (setq controller-recording (cons (controller-get "break") controller-recording))
  (message "Break inserted"))

(defun controller-page-down ()
  "Scroll Down."
  (interactive)
  (controller-execute "page_down"))

(defun controller-page-up ()
  "Scroll Up."
  (interactive)
  (controller-execute "page_up"))

(defun controller-scroll-left ()
  "Scroll Left."
  (interactive)
  (controller-execute "scroll_left"))

(defun controller-scroll-right ()
  "Scroll Right."
  (interactive)
  (controller-execute "scroll_right"))

(defun controller-scroll-up ()
  "Scroll Left."
  (interactive)
  (controller-execute "scroll_up"))

(defun controller-scroll-down ()
  "Scroll Right."
  (interactive)
  (controller-execute "scroll_down"))

(defun controller-refresh ()
  "Scroll Down."
  (interactive)
  (controller-execute "refresh"))

(defun controller-input-mode (input)
  "Start Input Mode"
  (interactive "sInput: ")
  (controller-execute "send_input" input))

(defun controller-url-goto (input)
  "Start Input Mode"
  (interactive "sURL: ")
  ;; FIXME This should be in the text box. Too lazy to figure out how that works
  (if (eq t (compare-strings "http" 0 3 input 0 3))
      (controller-execute "url_nav" input)
    (controller-execute "url_nav" (format "https://%s" input))))

(defun controller-new-tab ()
  "Open new tab"
  (interactive)
  (controller-execute "new_tab"))

(defun controller-send-enter ()
  "Send Enter."
  (interactive)
  (controller-execute "send_enter"))

(defun controller-close-tab ()
  "Send Enter."
  (interactive)
  (controller-execute "close_tab"))

(defun controller-send-escape ()
  "Send Escape."
  (interactive)
  (controller-execute "send_escape"))

(defun controller-send-tab ()
  "Send Escape."
  (interactive)
  (controller-execute "send_tab"))

(defun controller-backward-history ()
  "Go Back."
  (interactive)
  (controller-execute "backward_history"))

(defun controller-forward-history ()
  "Go Forward."
  (interactive)
  (controller-execute "forward_history"))

(defun controller-search-in-page (input)
  "Search for element"
  (interactive "sText: ")
  (controller-execute "search_in_page" input)
  (controller-highlight))

(defun controller-guided-search-in-page ()
  "Like search but with helm"
  (interactive)
  (setq options (split-string (car (controller-execute "guided_search" nil t)) "\n"))
  (setq some-helm-source
	'((name . "Guided Search")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-search-in-page candidate)))))
  (helm :sources '(some-helm-source)))


(defun controller-click-highlighted ()
  "Click highlighted Element."
  (interactive)
  (controller-execute "click_highlighted"))

(defun controller-print-highlighted ()
  "Print the text of this element."
  (interactive)
  (controller-execute "print_highlighted"))

(defun controller-bookmark-page ()
  "Bookmark the current page"
  ;; TODO: Names and descriptions
  (interactive)
  (setq controller-bookmarks (cons (controller-execute "current_url" nil t) controller-bookmarks)))

(defun controller-highlight ()
  "Highlight Found Element."
  (interactive)
  (controller-execute "highlight_element" nil t))

(defun controller-resize-browser ()
  "Resize the browser window."
  (interactive)
  (controller-execute "resize_browser"))

(defun controller-wait-for ()
  "Highlight elements on the page and open a helm window for selection"
  (interactive)
  (controller-execute "start_find")
  (setq options (reverse (split-string (car (controller-execute "return_keys" nil t)))))
  (setq some-helm-source
	'((name . "Choose Element")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-marker-focus candidate)))))
  (progn
    (helm :sources '(some-helm-source))
    (if controller-is-recording
	(controller-wait-chooser)))
  (controller-quit-find)
  (controller-highlight))

 
(defun controller-find ()
  "Highlight elements on the page and open a helm window for selection"
  (interactive)
  (controller-execute "start_find")
  (setq options (reverse (split-string (car (controller-execute "return_keys" nil t)))))
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
  (controller-highlight))

(defun controller-quit-find ()
  "Quit find mode"
  (interactive)
  (controller-execute "quit_find" nil t))

(defun controller-switch-tab ()
  "switch tabs"
  (interactive)
  (setq options (controller-execute "switch_tab"))
  (setq some-helm-source
	'((name . "Choose Tab")
          (candidates . options)
          (action . (lambda (candidate)
                      (controller-switch-tab-switch candidate)))))
  (helm :sources '(some-helm-source)))

(defun controller-get-source ()
  "Gets the source of the current page."
  (interactive)
  (controller-execute "get_page_source" nil t)
  (with-output-to-temp-buffer (generate-new-buffer  "*controller-page-source*")
    (switch-to-buffer-other-window (generate-new-buffer "*controller-page-source*"))
    (insert (controller-execute "get_page_source" nil t))
    (html-mode)))

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
      (message "Recording!"))))

;;; Provide
(provide 'controller)

;;; controller.el ends here
