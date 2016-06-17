(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defmacro uncomment (body) body)

(require 'package)
(package-initialize)

(setq my-user-emacs-directory "~/.emacs.d/")

;; bootstrap: https://github.com/jwiegley/use-package
(load-file (concat my-user-emacs-directory "bind-key.el"))
(load-file (concat my-user-emacs-directory "use-package.el"))

(setq package-archives '(("tromey" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;;("melpa-milkbox" . "http://melpa.milkbox.net/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))

(dolist (file '("load/basic.el"
		"load/directories.el"
		"load/backup-dir.el"
		"load/backup-dir-conf.el"
		"load/built-in.el"
                "load/util-fns.el"
                "load/ido-conf.el"
                "load/popwin-conf.el"
                "load/tramp-conf.el"
                ;;"load/highlight-flash-conf.el"
                "load/lisp-conf.el"
                ;;"load/js.el"
		))
  (load-file (concat my-user-emacs-directory file)))

(use-package diminish
  :ensure t)

(use-package nyan-mode
  :ensure t
  ;;:init (nyan-start-music)
  ;; (nyan-stop-music)
  )

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config

  (custom-set-variables
   '(haskell-process-path-ghci "env")
   '(haskell-process-type (quote ghci))
   '(haskell-process-suggest-haskell-docs-imports nil)
   '(haskell-process-args-ghci
     (quote
      ("DYLD_INSERT_LIBRARIES=/System/Library/Frameworks/GLUT.framework/GLUT"
       "stack" "ghci" "--ghci-options" "-fno-ghci-sandbox"))))

  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  ;(add-hook 'haskell-mode-hook 'flycheck-mode)
  )

(use-package input
  :bind (("C-\\" . toggle-input-method)))

(use-package image+
  :ensure t
  :config (imagex-global-sticky-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (concat live-etc-dir "multiple-cursors-prefs.el"))
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-M-z" . mc/edit-lines)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-o" . ace-jump-mode)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-branch-and-checkout)
         ("C-c g c" . magit-checkout)
         ("C-c g l" . magit-log-all)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind (("C-M-_" . undo-tree-undo)
         ("C-_" . undo-tree-undo)))

(use-package flycheck
 :ensure t
 :config
 (add-to-list 'flycheck-disabled-checkers 'emacs-lisp)
 (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
 (global-flycheck-mode))

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

(use-package markdown-toc
  :ensure t)

(use-package erlang
  :ensure t)

(use-package elpy
  :ensure t
  :pin elpy
  :config
  (elpy-enable)
  (elpy-use-ipython)
  (define-key python-mode-map (kbd "C-c C-l") 'elpy-shell-send-region-or-buffer)
  (define-key python-mode-map (kbd "C-c C-;") 'elpy-shell-send-current-statement)
  )

(use-package ein
  ;; https://github.com/millejoh/emacs-ipython-notebook
  :ensure t)

(use-package idris-mode
  :ensure t)

(use-package ess
  :ensure t
  :pin melpa-stable)

;; (use-package ess-view
;;   :ensure t)

;; (use-package ess-R-data-view
;;   :ensure t)

(use-package julia-mode
  :ensure t)

(use-package latex
  :ensure auctex
  :config
  (setq preview-scale-function 1.5)
  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-command-force "LatexMk")
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b -g %n %o %b"))))

(use-package auctex-latexmk
  :ensure t
  :config (auctex-latexmk-setup))

;; (use-package latex-preview-pane
;;   :ensure t
;;   :config
;;   (latex-preview-pane-enable))

(use-package gnuplot-mode
  :ensure t)

(c-add-style "llvm.org"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)))))

(use-package other
  :bind (("C-c o" . ff-find-other-file)))

(use-package c
  :config
  (setq c-default-style "linux"
        c-tab-always-indent t
        c-indent-tabs-mode t
        c-basic-offset 4)
  (defun how-many-region (begin end regexp &optional interactive)
    "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
    (interactive "r\nsHow many matches for (regexp): \np")
    (let ((count 0) opoint)
      (save-excursion
        (setq end (or end (point-max)))
        (goto-char (or begin (point)))
        (while (and (< (setq opoint (point)) end)
                    (re-search-forward regexp end t))
          (if (= opoint (point))
              (forward-char 1)
            (setq count (1+ count))))
        (if interactive (message "%d occurrences" count))
        count)))
  (defun v--infer-indentation-style ()
    ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
    ;; neither, we use the current indent-tabs-mode
    (let ((space-count (how-many-region (point-min) (point-max) "^  "))
          (tab-count (how-many-region (point-min) (point-max) "^\t")))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t))))
  (add-hook 'c-mode-hook '(lambda ()
                            (progn
                              (setq indent-tabs-mode nil)
                              (v--infer-indentation-style)))))



(comment
 (use-package cmake-font-lock
   :ensure t))

(use-package dash-at-point
  :ensure t
  :config
  (add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp"))
  :bind (("C-c d" . dash-at-point)))

;; (use-package irony
;;   :ensure t
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony)))

(use-package swift-mode
  :ensure t
  :config
  (add-to-list 'flycheck-checkers 'swift))

(use-package php-mode
  :ensure t)

(use-package win-switch
  :ensure t
  :bind ("C-x o" . win-switch-dispatch)
  :config
  (setq win-switch-feedback-background-color "DeepPink3")
  (setq win-switch-feedback-foreground-color "black"))

(use-package window-number
  :ensure t
  :diminish window-number-mode
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

(use-package dired
  :config
  (setq dired-dwim-target t))

(use-package go-mode
  :ensure t)

(use-package fstar-mode
  :ensure t)

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$"
  :ensure t)

(use-package glsl-mode
  :mode "\\.(glsl|vert|frag|geom)$"
  :ensure t)

;(use-package plantuml-mode
;  :ensure t
;  ;; wants to know about plantuml.jar)

;; themes

(defun cycle-my-theme ()
  "Cycle through a list of themes, my-themes."
  (interactive)
  (when curr-theme
    (disable-theme curr-theme)
    (setq my-themes (append my-themes (list curr-theme))))
  (setq curr-theme (pop my-themes))
  (load-theme curr-theme t))

(defvar curr-theme nil)

;;(defvar my-themes '(cyberpunk flatui leuven))
;;(defvar my-themes '(cyberpunk flatui leuven))

;; (use-package naquadah-theme :ensure t)
;; (use-package hemisu-theme :ensure t)
;; (use-package monokai-theme :ensure t)
;; (use-package molokai-theme :ensure t)
;; (use-package ir-black-theme :ensure t)

(comment
 (use-package flatui-theme :ensure
   :config
   (custom-set-faces
      '(default ((t (:background "#eee"))))) ; mainly terminal background
   ))
(comment
 (use-package cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk)
  (custom-set-faces
   '(default ((t (:background "#000"))))) ; mainly terminal background
  ))

(uncomment
 (use-package leuven-theme
   :ensure t
   :config
   (custom-set-faces
    '(default ((t (:background "#FFFFFE"))))) ; mainly terminal background
   ))

;;(cycle-my-theme)

(uncomment
;; light:
 (custom-set-faces
  '(shm-quarantine-face ((t (:inherit font-lock-error))))
  '(shm-current-face ((t (:background "#efefef"))))))

(comment
 (custom-set-faces
 '(shm-quarantine-face ((t (:inherit font-lock-error))))
  '(shm-current-face ((t (:background "#30344A"))))))

;; (use-package smex
;;   :ensure t
;;   :config (smex-initialize)
;;   :bind (("M-x" . smex)
;;          ("M-X" . smex-major-mode-commands)))

;; (use-package browse-kill-ring
;;   :ensure t
;;   :bind (("M-y" . browse-kill-ring)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-recency))))
  (setq projectile-switch-project-action 'projectile-dired))

(define-key comint-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(use-package helm
  :ensure t
  :diminish helm-mode
  :config (setq helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match    t
                helm-split-window-default-side 'other)

  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

  :init (progn
          (use-package helm-swoop
            :ensure t
            :demand
            :config (setq helm-multi-swoop-edit-save t
                          helm-swoop-split-with-multiple-windows nil
                          helm-swoop-split-direction 'split-window-vertically
                          helm-swoop-speed-or-color nil
                          helm-swoop-move-to-line-cycle t
                          helm-swoop-use-line-number-face t
                          helm-c-source-swoop-search-functions
                          '(helm-mm-exact-search
                            helm-mm-search
                            helm-candidates-in-buffer-search-default-fn
                            helm-fuzzy-search))

            (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
            (define-key isearch-mode-map (kbd "C-o") 'helm-swoop-from-isearch)

            :bind (("M-i" . helm-swoop)
                   ("M-I" . helm-swoop-back-to-last-point)

                   ("C-c M-i" . helm-multi-swoop)
                   ("C-c M-I" . helm-multi-swoop-all)))

          (use-package helm-projectile
            :ensure t
            :demand
            :config
            (helm-projectile-on)
            (setq projectile-switch-project-action 'projectile-dired)
            (setq helm-for-files-preferred-list
                  '(;;helm-source-buffers-list
                    helm-source-projectile-buffers-list
                    helm-source-projectile-files-list
                    helm-source-projectile-recentf-list
                    helm-source-recentf
                    ;;helm-source-files-in-current-dir
                    ))
            :bind (("C-x f" . helm-for-files)
                   ;;("C-c p p" . helm-projectile-switch-project)
                   ("C-x C-M-f" . helm-projectile-find-file-dwim)))

          (use-package helm-ag
            :ensure t
            :bind (("C-c a" . helm-projectile-ag)))

          (use-package helm-descbinds
            :ensure t
            :config (helm-descbinds-mode)))

  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x j" . helm-imenu)
         ("C-x p" . helm-etags-select)
         ("M-y" . helm-show-kill-ring)
         ("M-X" . helm-resume)
         ("M-o" . helm-mini)
         ;;("M-O" . helm-resume)
         ;;("C-h SPC" . helm-all-mark-rings)
         ))

(use-package browse-at-remote
  :ensure t
  :bind (("C-c g g" . browse-at-remote)))

(add-to-list 'load-path "~/.emacs.d/vendor")
(load "~/.emacs.d/vendor/PG/generic/proof-site")

(use-package company-coq
  :ensure t
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (add-to-list 'flycheck-disabled-checkers 'coq)
  :bind (("C-c C-'" . proof-assert-until-point-interactive)))

(use-package nix-mode
  ;:ensure t
  :init

  (defun nix-eval-apply-ansi-color (proc string)
    "Filter to function for process PROC to apply ansi color to STRING."
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (progn
            ;; Insert the text, advancing the process marker.
            (goto-char (process-mark proc))
            (insert string)
            (ansi-color-apply-on-region (process-mark proc) (point))
            (set-marker (process-mark proc) (point)))))))

  (defun nix-eval ()
    "eval nix attribute at point, must be top-level to the current file"
    (interactive)
    (let ((cmd
           (format "nix-instantiate -I %s/deps --show-trace --eval --strict --json -A %s %s | jq -r ."
                   (projectile-project-root) (thing-at-point 'word) (buffer-file-name))))
      (with-current-buffer (get-buffer-create "*nix-eval*")
        (message cmd)
        (start-process-shell-command "nix-eval" (current-buffer) cmd)
        (set-process-filter (get-process "nix-eval") 'nix-eval-apply-ansi-color)
        (switch-to-buffer-other-window (current-buffer)))))

  :config
  (flycheck-define-checker nix
    "Nix checker"
    :command ("nix-instantiate"
              "--parse-only"
              source)
    :error-patterns
    ((error line-start "error:" (message) " at " (file-name) ":" line ":" column line-end))
    :modes nix-mode)

  (define-key nix-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key nix-mode-map (kbd "M-p") 'flycheck-previous-error)
  (define-key nix-mode-map (kbd "TAB") 'nix-indent-line)

  (add-to-list 'flycheck-checkers 'nix))

;(define-key sh-mode-map (kbd "M-n") 'flycheck-next-error)
;(define-key sh-mode-map (kbd "M-p") 'flycheck-previous-error)

(defun shell-region (command)
  "execute region in an inferior shell"
  (interactive (list (read-shell-command "Bash on region: ")))
  (shell-command-on-region (region-beginning) (region-end) (format "%s %s \"%s\"" "bash" "-c" command)))

(defun my/fork-window-to-frame ()
  "move the current window's buffer to another frame and kill the window"
  (interactive)
  (let ((current (selected-window)))
    (make-frame)
    (delete-window current)))

(defun my/projectile-pop-to-shell (arg)
  "pop to a shell identified with a current project"
  (interactive "p")

  (let ((name (let ((suffix (if (= 1 arg) "" (format "-%s" arg))))
                (format "%%%s%s%%" (projectile-project-name) suffix))))
    (pop-to-buffer name t)
    (shell name)))

(global-set-key (kbd "C-M-<return>") 'my/projectile-pop-to-shell)
(global-set-key (kbd "C-x 5 5") 'my/fork-window-to-frame)

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; (use-package bash-completion
;;   :ensure t ;; ???
;;   :config (bash-completion-setup))

;;increment and decrement number at point
(global-set-key (kbd "C-M-_")  'live-decrement-number-at-point)
(global-set-key (kbd "M-=")    'live-increment-number-at-point)
(global-set-key (kbd "C-M-=")    'live-increment-number-at-point)

;;make C-] and M-] cut and copy respectively
(global-set-key (kbd "C-]") 'kill-region)
(global-set-key (kbd "M-]") 'kill-ring-save)

;;mark current function
(global-set-key (kbd "C-x C-p") 'mark-defun)

;;use delete-horizontal-space to completely nuke all whitespace
(global-set-key (kbd "M-SPC ") 'live-delete-whitespace-except-one)

;;make ^h delete rather than help
(global-set-key (kbd "C-h") 'delete-backward-char)
;(define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)

;;redefine help shortcut
(global-set-key (kbd "M-h") 'help-command)

;;kill line backwards
(global-set-key (kbd "M-k") 'live-backwards-kill-line)

;;set the mark
(global-set-key (kbd "C-SPC") 'set-mark-command)

;;repeat previous command
(global-set-key (kbd "M-'") 'repeat)

;;scroll other window
(global-set-key (kbd "C-M-]") 'scroll-other-window)
(global-set-key (kbd "C-M-[") 'scroll-other-window-down)

;;fast vertical naviation
(global-set-key  (kbd "M-U") (lambda () (interactive) (forward-line -10)))
(global-set-key  (kbd "M-D") (lambda () (interactive) (forward-line 10)))
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-s")   'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)



(defun my/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(define-key comint-mode-map (kbd "C-c C-k") 'my/comint-clear-buffer)

;(set ffap-machine-p-known 'reject)

(global-set-key (kbd "C--")         'comment-or-uncomment-region)

(global-set-key (kbd "M-`")         'other-frame)
(global-set-key (kbd "<C-tab>")     'ibuffer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(defun my/org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))
(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)

(define-key global-map (kbd "C-0") 'terminal-here)

(defun terminal-here ()
  (interactive)
  (dired-smart-shell-command "open -a Terminal $PWD" nil nil))

(use-package yaml-mode
  :ensure t)

;; (Uncomment
;;  (use-package deferred
;;    :ensure t
;;    :config (require 'deferred))

;; (uncomment
;;  (use-package org-trello
;;    :ensure t))

;; (set-frame-parameter (selected-frame) 'alpha '(95 95))
;; (set-frame-parameter (selected-frame) 'alpha '(100 100))

(use-package cider
  :pin melpa-stable
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljc"))
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs"))
  (add-to-list 'dumb-jump-find-rules
               '(:type "function" :language "clojure" :regex "\\\(rum/defcs?\\s+JJJ\\j"))

  (add-to-list 'dumb-jump-language-file-exts '(:language "haskell" :ext "hs"))
  (add-to-list 'dumb-jump-find-rules
               '(:type "function" :language "haskell" :regex "^\\s+JJJ\\j"))
  (dumb-jump-mode))

(use-package sml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode)))

;; (use-package cider-eval-sexp-fu
;;   :pin melpa-stable
;;   :ensure t)

;; (package-refresh-packages)
