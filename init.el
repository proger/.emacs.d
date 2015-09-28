;;; wtf

(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ;("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(package-refresh-contents)

(dolist (pkg '(ace-jump-mode
               ag
               auto-complete
               browse-kill-ring
               erlang
               eval-sexp-fu
               evil
               exec-path-from-shell
               expand-region
               flx
               flx-ido
               flycheck
               flycheck-haskell
               ghc
               haskell-mode
               highlight
               ido
               idomenu
               js-comint
               js2-mode
               magit
               markdown-mode
               multiple-cursors
               nix-mode
               nodejs-repl
               paredit
               popwin
               projectile
               rainbow-delimiters
               smex
               stupid-indent-mode
               swift-mode
               undo-tree
               undo-tree
               web-mode
               win-switch ; (win-switch-dispatch)
               window-number))
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(projectile-global-mode)
(set-fringe-mode 1) ; minimal

(setq evil-default-state 'emacs)
(require 'evil)
(evil-mode t)

(load "haskell-mode-autoloads")

(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(change-major-mode-with-file-name t)
 '(global-mark-ring-max 17)
 '(haskell-doc-prettify-types (quote nil))
 '(haskell-indentation-show-indentations (quote nil))
 '(haskell-interactive-popup-errors (quote nil))
 '(haskell-process-args-ghci
   (quote
    ("NIX_PATH=/Users/vladki/src/eris/deps" "with-aws" "stack" "ghci")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "env")
 '(haskell-process-type (quote ghci))
 '(haskell-process-use-presentation-mode t)
 '(haskell-process-suggest-remove-import-lines nil))

(setq ghc-ghc-options '("-isrc"))
(setq ghc-debug t)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (haskell-hook)))

; compat
(defun turn-off-haskell-font-lock ()
  "Turns off font locking in current buffer."
  (font-lock-mode -1))

(defun haskell-hook ()
  ;(turn-on-haskell-doc-mode)

  (set-face-attribute 'ghc-face-error nil :underline nil)
  (set-face-attribute 'ghc-face-warn nil :underline nil)

  (define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
  (define-key haskell-mode-map (kbd "C-c C-g") 'ghc-show-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-.")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))

  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-,")
    (lambda ()
      (interactive)
      (haskell-move-nested -1))))

(put 'dired-find-alternate-file 'disabled nil)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save))

(flycheck-define-checker nix
  "Nix checker"
  :command ("nix-instantiate"
            "--parse-only"
            source)
  :error-patterns
  ((error line-start "error:" (message) ", at " (file-name) ":" line ":" column line-end))
  :modes nix-mode)

(add-to-list 'flycheck-checkers 'nix)
(add-to-list 'flycheck-checkers 'swift)


(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-recency))))

(setq projectile-switch-project-action 'projectile-dired)

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun evil-backward-delete-word-or-kill-region ()
  "Delete word backward like vim or kill region if active"
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (evil-delete-backward-word)))

(global-set-key (kbd "C-x C-M-f") 'projectile-find-file)
(global-set-key (kbd "C-x f")     'live-recentf-ido-find-file)
(global-set-key (kbd "C-c p g")   'projectile-ag)
(global-set-key (kbd "C-c p h")   'projectile-ag)

(global-set-key (kbd "C--")       'comment-or-uncomment-region)

(global-set-key (kbd "M-`")     'other-frame)
(global-set-key (kbd "<C-tab>")     'ibuffer)

(global-set-key (kbd "C-x <left>")     'switch-to-buffer)
(global-set-key (kbd "C-x <right>")     'switch-to-buffer)

(global-set-key (kbd "<M-backspace>") 'backward-kill-line)
(global-set-key (kbd "C-w") 'evil-backward-delete-word-or-kill-region)

(require 'ido)
;(define-key ido-file-completion-map (kbd "C-w") 'evil-backward-delete-word-or-kill-region)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 130))

  ; same as command
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (setq ns-use-native-fullscreen nil))

(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(setq tramp-default-method "ssh")
;(add-to-list 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;(require 'stupid-indent-mode)
;(add-hook 'nix-mode-hook 'stupid-indent-mode)

(require 'epa-file)
(epa-file-enable)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
;(setq epa-armor t)


(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
	   (fullpath (concat directory "/" path))
	   (isdir (car (cdr element)))
	   (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
	((and (eq isdir t) (not ignore-dir))
	 (load-directory fullpath))
	((and (eq isdir nil) (string= (substring path -3) ".el"))
	 (load (file-name-sans-extension fullpath)))))))

(setq
  live-root-dir     user-emacs-directory
  live-tmp-dir      (file-name-as-directory (concat live-root-dir "tmp"))
  live-etc-dir      (file-name-as-directory (concat live-root-dir "etc"))
  live-pscratch-dir (file-name-as-directory (concat live-tmp-dir  "pscratch"))
  live-lib-dir      (file-name-as-directory (concat live-root-dir "lib"))
  live-packs-dir    (file-name-as-directory (concat live-root-dir "packs"))
  live-autosaves-dir(file-name-as-directory (concat live-tmp-dir  "autosaves"))
  live-backups-dir  (file-name-as-directory (concat live-tmp-dir  "backups"))
  live-custom-dir   (file-name-as-directory (concat live-etc-dir  "custom"))
  live-load-pack-dir nil
  live-disable-zone t)

;; create tmp dirs if necessary
(make-directory live-etc-dir t)
(make-directory live-tmp-dir t)
(make-directory live-autosaves-dir t)
(make-directory live-backups-dir t)
(make-directory live-custom-dir t)
(make-directory live-pscratch-dir t)

(setq mc/list-file (concat live-etc-dir "multiple-cursors-prefs.el"))
(require 'multiple-cursors)

(setq custom-file (concat live-custom-dir "custom-configuration.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(setq inferior-js-program-command "env NODE_NO_READLINE=1 node --interactive")
(add-hook 'js2-mode-hook '(lambda ()
                            (define-key js2-mode-map "\C-x\C-e" 'nodejs-repl-eval-dwim)))


(load-directory (concat user-emacs-directory "load"))


;; {ok, emacs}.
