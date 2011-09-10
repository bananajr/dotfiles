(provide 'choose-mode)

;;;======================================================================
;;; utility functions
;;;======================================================================
(defun file-name-with-new-extension (file ext)
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

(defun matlab-ish-buffer-p ()
  (or (buffer-contains-regexp "^[ \t]*function[ \t]")
      (buffer-contains-regexp "^[ \t]*%")))

(defun objc-ish-buffer-p ()
  (buffer-contains-regexp "@implementation"))

(defun matlab-mode-if-available-or-objc ()
  (if (functionp 'matlab-mode)
      (matlab-mode)
    (message "file appears to be matlab code but matlab-mode is not available")
    (objc-mode)))

(defun m-delta-from-mbuffer ()
  (cond ((objc-ish-buffer-p) 1)
        ((matlab-ish-buffer-p)  -1)))

(defun m-delta-from-hbuffer () 1)
(defun m-delta-from-cbuffer () -1)

(defun matlab-or-objc-from-count (count)
  (cond ((not count) nil)
        ((< count 0) (matlab-mode-if-available-or-objc) t)
        ((> count 0) (objc-mode) t)
        (t nil)))

(defun matlab-or-objc-from-corresponding-hfile (file)
  (matlab-or-objc-from-count (visit-file-for-fn file 'm-delta-from-hbuffer)))

(defun matlab-or-objc-from-mbuffer()
  (matlab-or-objc-from-count (m-delta-from-mbuffer)))

(defun matlab-or-objc-from-other-files ()
  (message "checking other files")
  (let ((bufdir (file-name-directory (buffer-file-name)))
        (count 0) ; positive for objc, negative for matlab
        (matlab-or-objc-ext-delta-map
         '(("h" . m-delta-from-hbuffer)
           ("m" . m-delta-from-mbuffer)
           ("c" . m-delta-from-cbuffer)))
    (dolist (file (directory-files dir) count)
      (dolist (map matlab-or-objc-ext-delta-map count)
        (when (equal (file-name-extension file) (car map))
          (setq count (+ (visit-file-for-fn file (cdr map)) count)))))
    (matlab-or-objc-from-count count))))

(defun choose-mode-for-dot-m ()
  (cond ((matlab-or-objc-from-corresponding-hfile 
          (file-name-with-new-extension (buffer-file-name) ".h")) t)
        ((matlab-or-objc-from-mbuffer) t)
        ((matlab-or-objc-from-other-files) t)
        (t (objc-mode) t)))
