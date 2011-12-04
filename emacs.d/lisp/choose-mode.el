(provide 'choose-mode)

;;;======================================================================
;;; utility functions
;;;======================================================================
(defun filename-with-new-extension (file ext)
  (unless (char-equal (aref ext 0) ?.)
    (setq ext (concat "." ext)))
  (concat (file-name-sans-extension file) ext))

(defun buffer-contains-regexp (regexp)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp regexp nil t)))

(defun visit-file-for-fn (file func &rest args)
  (if (not (file-exists-p file))
      nil
    (let ((buf (create-file-buffer file))
          result)
      (set-buffer buf)
      (insert-file-contents file nil nil nil t)
      (setq result (apply func args))
      (kill-buffer buf)
      result)))

(defun num-files-in-dir-with-ext (dir ext)
  (let ((count 0))
    (dolist (fnm (directory-files dir) count)
      (when (equal (file-name-extension fnm) ext) 
        (setq count (+ 1 count))))))

(defun other-files-in-buffer-dir-with-ext-p (ext)
  (let ((bufdir (file-name-directory (buffer-file-name))))
    (> (num-files-in-dir-with-ext bufdir ext) 0)))

(defun choose-mode (mode)
  ;(message "choosing %s" (prin1-to-string mode))
  (apply mode ())
  t)


;;;======================================================================
;;; .h: distinguish between c, c++, and objective-c header files
;;;======================================================================
(defun choose-mode-for-dot-h ()
 (when (string-match "\\(.*\\)\.h$" (buffer-file-name))
   (let ((base_str (match-string 1 (buffer-file-name))))

     ;; first see if there's a corresponding implemetation file
     (unless
         (cond ((file-exists-p (concat base_str ".c")) (c-mode) t)
               ((file-exists-p (concat base_str ".cc")) (c++-mode) t)
               ((file-exists-p (concat base_str ".cpp")) (c++-mode) t)
               ((file-exists-p (concat base_str ".cxx")) (c++-mode) t)
               ((file-exists-p (concat base_str ".m")) (objc-mode) t)
               ((file-exists-p (concat base_str ".mm")) (objc-mode) t))

       ;; if not, search the buffer for hints
       (message "no implementation file; searching for hints")
       (cond ((buffer-contains-regexp "@interface") (objc-mode))
             ((buffer-contains-regexp "^[ \t]*class") (c++-mode))
             (t (c-mode)))))))

(push '("\\.h\\'" . choose-mode-for-dot-h) auto-mode-alist)



;;;======================================================================
;;; .m: distinguish between objective-c and matlab files
;;;======================================================================

(defun matlab-ish-mbuffer-p ()
  ;(message "checking for matlab-ish .m buffer")
  (or (buffer-contains-regexp "^[ \t]*function[ \t]")
      (buffer-contains-regexp "^[ \t]*%")))

(defun objc-implementation-ish-mbuffer-p ()
  (or (buffer-contains-regexp "@implementation")
      (buffer-contains-regexp "@import")
      (buffer-contains-regexp "@NSString")))

(defun matlab-mode-if-available ()
  (if (functionp 'matlab-mode)
      (choose-mode 'matlab-mode)
    ;(message "matlab-mode is unavailable")
    nil))

(defun matlab-ish-mbuffer-check ()
  ;(message "checking for matlab-ish .m file")
  (if (not (matlab-ish-mbuffer-p)) nil
    (message "matlab-ish buffer detected");
    (matlab-mode-if-available)))

(defun objc-implementation-ish-mbuffer-check ()
  ;(message "checking for objc-ish .m file")
  (if (not (objc-implementation-ish-mbuffer-p)) nil
    (message "objc-ish buffer detected")
    (choose-mode 'objc-mode)))

(defun objc-corresponding-hfile-check ()
  ;(message "checking for objc-ish corresponding .h file")
  (if (not (file-exists-p (filename-with-new-extension (buffer-file-name) ".h")))
      nil
    (message "corresponding .h file detected")
    (choose-mode 'objc-mode)))

(defun apply-objc-matlab-last-mode ()
  (if 'objc-matlab-last-mode
      (progn
        (message "applying last mode (%s)"
                 (prin1-to-string objc-matlab-last-mode))
        (apply objc-matlab-last-mode ()))
    (message "choose-mode-for-dot-m: no objc-matlab-last-mode to apply")))

(defun choose-mode-for-dot-m ()
  (or
   ; check for mode-specific keywords
   (matlab-ish-mbuffer-check)
   (objc-implementation-ish-mbuffer-check)
   ; if the corresponding header file is empty, assume objc
   (objc-corresponding-hfile-check)
   ; run the most recently used mode
   (apply-objc-matlab-last-mode)))

(defun choose-mode-matlab-mode-hook ()
  ;(message "objc-matlab-last-mode = matlab-mode")
  (setq objc-matlab-last-mode 'matlab-mode))
(add-hook 'matlab-mode-hook 'choose-mode-matlab-mode-hook)

(defun choose-mode-objc-mode-hook ()
  ;(message "objc-matlab-last-mode = objc-mode")
  (setq objc-matlab-last-mode 'objc-mode))
(add-hook 'objc-mode-hook 'choose-mode-objc-mode-hook);

