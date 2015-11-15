(require 'package)
(package-initialize)
;; bootstrap: https://github.com/jwiegley/use-package
(load-file (concat user-emacs-directory "bind-key.el"))
(load-file (concat user-emacs-directory "use-package.el"))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(dolist (file '("load/osx.el"
		"load/basic.el"
		"load/directories.el"
		"load/backup-dir.el"
		"load/backup-dir-conf.el"
		"load/built-in.el"
                "load/util-fns.el"
                "load/ido-conf.el"
                "load/live.el"
                "load/default-bindings.el"
                "load/popwin-conf.el"
                "load/tramp-conf.el"
                "load/highlight-flash-conf.el"
                "load/lisp-conf.el"
                "load/python.el"
                "load/nix.el"
                "load/js.el"
		"load/haskell.el"))
  (load-file (concat user-emacs-directory file)))

(use-package ag
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-recency))))
  (setq projectile-switch-project-action 'projectile-dired)
  :bind (("C-x C-M-f" . projectile-find-file)
         ("C-c p g"   . projectile-ag)
         ("C-c p h"   . projectile-ag)))

(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (concat live-etc-dir "multiple-cursors-prefs.el"))
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-o" . ace-jump-mode)))

(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :ensure t
  :bind (("C-M-_" . undo-tree-undo)
         ("C-_" . undo-tree-undo)))

(use-package flycheck
  :ensure t)

(use-package paredit
  :ensure t)

(use-package puppet-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package win-switch
  :ensure t
  :bind ("C-x o" . win-switch-dispatch)
  :config
  (setq win-switch-feedback-background-color "DeepPink3")
  (setq win-switch-feedback-foreground-color "black"))

(use-package window-number
  :ensure t
  :init
  (autoload 'window-number-mode "window-number"
    "A global minor mode that enables selection of windows according to numbers with the C-x C-j prefix.  Another mode, `window-number-meta-mode' enables the use of the M- prefix."
    t)
  (autoload 'window-number-meta-mode "window-number"
    "A global minor mode that enables use of the M- prefix to select windows, use `window-number-mode' to display the window numbers in the mode-line."
    t)
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1))

(use-package epa-file
  :config
  (epa-file-enable)
  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  (epa-file-name-regexp-update)
  ;(setq epa-armor t)
  )

(use-package winner
  :bind
  (("C-c b" . winner-undo)
   ("C-c f" . winner-redo)))

(global-set-key (kbd "C--")         'comment-or-uncomment-region)

(global-set-key (kbd "M-`")         'other-frame)
(global-set-key (kbd "<C-tab>")     'ibuffer)

(global-set-key (kbd "C-x <left>")  'switch-to-buffer)
(global-set-key (kbd "C-x <right>") 'switch-to-buffer)

(use-package hemisu-light
	     :ensure t
	     :config (load-theme 'hemisu-light))
