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

(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t)

;; set all coding systems to utf-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

;; make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(setq font-lock-maximum-decoration t
      color-theme-is-global t)

;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)

;; remove bells
(setq ring-bell-function 'ignore)

;; font setitng functions
(require 'cl)

(defun live-set-default-font (font-string)
  "Sets the default font and sets all frames to the same font trying to maintain window resolution. Only changes font if window system is not a basic terminal."
  (interactive "MNew emacs live default font: ")
  (setq default-frame-alist
        (remove-if (lambda (x)
                     (eq 'font (car x)))
                   default-frame-alist))
  (cond
   ((member (window-system) '(x w32 ns))
    (add-to-list 'default-frame-alist (cons 'font font-string))
    (set-default-font font-string t t))))

(defun live-set-default-darwin-font (font-string)
  "Sets the default font and sets all frames to the same font trying to maintain window resolution. Only changes font if system-type is darwin in a window system."
  (interactive "MNew darwin default font: ")
  (cond
   ((eq system-type 'darwin)
    (live-set-default-font font-string))))

(live-set-default-darwin-font "Menlo-12")

;; make fringe smaller
(if (fboundp 'fringe-mode)
    (fringe-mode 4))
