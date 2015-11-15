(when (eq system-type 'darwin)
  (setq default-input-method "MacOSX")

  ;; Make cut and paste work with the OS X clipboard
  (defun live-copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun live-paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (when (not window-system)
    (setq interprogram-cut-function 'live-paste-to-osx)
    (setq interprogram-paste-function 'live-copy-from-osx))

  ;; Work around a bug on OS X where system-name is a fully qualified
  ;; domain name
  (setq system-name (car (split-string system-name "\\.")))

  ; big window by default
  (add-to-list 'default-frame-alist '(left . 60))
  (add-to-list 'default-frame-alist '(top . 20))
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 150))

  (setq ls-lisp-use-insert-directory-program t)
  (setq insert-directory-program "gls")

  ; same as command
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (setq ns-use-native-fullscreen nil))
