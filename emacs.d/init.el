;;;======================================================================
;;; .emacs
;;; jake sprouse <jsprouse@ri.cmu.edu>
;;; 
;;; 2006-02-17 initial version
;;;======================================================================

(message "Jake's .emacs")

;;; init stuff to do only if we're running under a console
(defun tty-init () nil)

;;; init stuff if we're running under a window system
(defun gui-init ()
  (message "requiring server")
  (require 'server)
  (tool-bar-mode 0)
  (setq initial-frame-alist
	`((background-color . "white")
	  (foreground-color . "black")
	  (cursor-color . "red")
	  (internal-border-width . 1)
	  (menu-bar-lines . 1)
	  (cursor-type . box)
	  )))

;;; if the font-spec is openable, it is returned, else nil
(defun font-spec-openablep(name)
  (if (find-font (font-spec :name name))
      name
    (message "Could not find font %s" name) nil))

;;; init stuff specifically if we're running on win32
(defun w32-select-font()
  (or (font-spec-openablep "Consolas-10")
      (font-spec-openablep "Courier New-10")))
(defun w32-init ()
  (message "M$ Windows-specific initialization")
  (setq font (w32-select-font))
  (message "Selected font %s for win32" font)
  (set-frame-font (w32-select-font))
  (when (and (= emacs-major-version 23)
	     (= emacs-minor-version 1))
    (defun server-ensure-safe-dir (dir)
      "Noop" (message "No-op") t)) ; supress EmacsW32 error "directory is unsafe"
  (server-start)
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

;;; init stuff specifically if we're running on mac
(defun mac-init ()
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'nil)
  (global-set-key "\C-x\M-v" 'clipboard-yank)
  (setq path-separator "/"))

;;; init stuff specifically if we're running on x11
(defun x11-init () nil)

  
(if (not window-system)
    (tty-init)
  (gui-init)
  (cond ((equal window-system 'w32) (w32-init))
		((equal window-system 'mac) (mac-init))
		(t (x11-init))))



;;;======================================================================
;;; default to $HOME
;;;======================================================================
(setq default-directory (file-name-as-directory (getenv "HOME")))



;;;======================================================================
;;; path for extra functions
;;;======================================================================
(setq load-path (append load-path (list "~/site-lisp")))


(set-foreground-color "black")
(set-background-color "white")
(set-cursor-color "red")
(set-mouse-color "blue")
(set-border-color "black")




;;;======================================================================
;;; keybindings
;;;======================================================================
(global-set-key "\C-xaa" 'align)
(global-set-key "\C-xar" 'align-regexp)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key "\C-xcc" 'compile)
(global-set-key "\C-xcn" 'next-error)
(global-set-key "\C-xO" 'ff-find-other-file)
(global-set-key "\C-x/" 'comment-region)
(global-set-key "\C-x\M-/" 'uncomment-region)
(global-set-key "\C-xM" '(message "boogers"))



;;;======================================================================
;;; compilation
;;;======================================================================
(setq compilation-read-command nil) ; don't prompt for compile command



;;;======================================================================
;;; viper mode on
;;;======================================================================
;(require 'vimpulse)
;(require 'redo)
(setq viper-mode t)
(load "viper")
(setq viper-want-ctl-h-help t)
;; viper is really unhelpful when it tries to check files out
(defun viper-maybe-checkout (buf)())



;;;======================================================================
;;; iswitchb mode on
;;;======================================================================
(setq iswitchb-mode t)



;;;======================================================================
;;; some utility functions
;;;======================================================================
(defun delete-previous-window ()
  (interactive)
  (delete-window (previous-window)))
(global-set-key (kbd "C-x 0") 'delete-previous-window)
(global-set-key (kbd "C-x 1") 'delete-window)

(defun scratch ()
  (interactive)
  (setq sb (get-buffer-create "*scratch*"))
  (setq sw (get-buffer-window sb))
  (if sw (select-window sw) (switch-to-buffer sb))
  (lisp-interaction-mode))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "M-i") 'indent-buffer)



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(inhibit-startup-screen t))


(setq c-default-style
      '((java-mode . "java")
	(awk-mode  . "awk")
	(c-mode    . "bsd")
	(c++-mode  . "bsd")
	(other     . "gnu")))



;;;======================================================================
;;; matlab
;;;======================================================================
(defun my-matlab-mode-hook ()
  (setq show-trailing-whitespace t)
  (setq fill-column (- (frame-width) 2)))
(if (or (featurep 'matlab) (load "matlab" t))
    (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
  (message "matlab.el[c] not found"))
;; we now handle .m files with choose-mode-for-dot-m
;;(setq auto-mode-alist (cons '("\\.m" . matlab-mode) auto-mode-alist))



;;;======================================================================
;;; c
;;;======================================================================
(defun my-c-mode-hook ()
  (setq show-trailing-whitespace t))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
;(add-hook 'c-mode-common-hook (lambda () (column-marker-1 79)))
(setq-default indent-tabs-mode nil)

;;;======================================================================
;;; objective-c
;;;======================================================================
(defun my-objc-mode-hook ()
  (setq show-trailing-whitespace t)
  (c-set-offset 'objc-method-args-cont 'c-lineup-ObjC-method-args)
  (c-set-offset 'objc-method-call-cont 'c-lineup-ObjC-method-call))
(add-hook 'objc-mode-hook 'my-objc-mode-hook);



;;;======================================================================
;;; add functions for choosing modes for ambiguous file extensions
;;;======================================================================
(require 'choose-mode)
(when (featurep 'choose-mode)
  (push '("\\.h\\'" . choose-mode-for-dot-h) auto-mode-alist)
  (push '("\\.m\\'" . choose-mode-for-dot-m) auto-mode-alist))


;; tab stuff
(setq tab-width 4)
(defun untabify-buffer () 
  "Replace all tabs with whitespace according to tab-width"
  (interactive)
  (untabify (point-min) (point-max)))



;; for R (statistical computing package)
;;(load "~/emacs/ess-5.2.0/lisp/ess-site")

;; for caml-mode
;;(setq load-path (append load-path (list "~/emacs/emacs-caml-mode")))
;;(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
;;(if window-system (require 'caml-font))


;;;======================================================================
;;; ESP
;;;======================================================================
(add-to-list 'auto-mode-alist '("\\.fsr\\'" . hexl-mode))



;;;======================================================================
;;; Git
;;;======================================================================
(if (or (featurep 'git) (load "git" t))
    (global-set-key "\C-xgs" 'git-status)
  (message "git.el[c] not found"))


;;;======================================================================
;;; font-lock
;;;======================================================================
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

(set-face-attribute 'font-lock-comment-face nil :foreground "dark green")
(set-face-attribute 'font-lock-keyword-face nil :foreground "dark red")
(set-face-attribute 'font-lock-builtin-face  nil :foreground "dark red")
(set-face-attribute 'font-lock-string-face   nil :foreground "dark blue")
(set-face-attribute 'font-lock-constant-face nil :foreground "DarkOrange3")
(set-face-attribute 'font-lock-type-face nil :foreground "unspecified")
(set-face-attribute 'font-lock-function-name-face nil :foreground "unspecified")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "unspecified")
(set-face-attribute 'trailing-whitespace nil :background "#ffd0d0")

;;;======================================================================
;;; set the current frame size
;;;======================================================================

(defun set-current-frame-width (w)
  (interactive "nWidth: ")
  (set-frame-width (selected-frame) w))
(global-set-key (kbd "C-x w") 'set-current-frame-width)

(defun set-current-frame-height (h)
  (interactive "nHeight: ")
  (set-frame-height (selected-frame) h))
(global-set-key (kbd "C-x h") 'set-current-frame-height)

(put 'narrow-to-region 'disabled nil)
