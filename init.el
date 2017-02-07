(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defmacro uncomment (body) body)

(require 'package)
(package-initialize)

(setq my-user-emacs-directory "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor")

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
                "load/built-in.el"
                "load/util-fns.el"
                ;;"load/ido-conf.el"
                "load/lisp-conf.el"
                "load/backup-dir.el"
                "load/backup-dir-conf.el"
                "load/tramp-conf.el"
                ;;"load/mit.el" ;; https://groups.csail.mit.edu/mac/users/gjs/6.945/dont-panic/
                ))
        (load-file (concat my-user-emacs-directory file)))


(use-package diminish
  :diminish auto-revert-mode
  :ensure t)

(use-package company
  :ensure t
  :config (global-company-mode))

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-reload-all)
;;   ;;(add-hook 'prog-mode-hook #'yas-minor-mode)
;;   )

;; (use-package nyan-mode
;;   :ensure t
;;   ;;:init (nyan-start-music)
;;   ;; (nyan-stop-music)
;;   )

(use-package prolog
  :config
  (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
  (setq prolog-system 'swi))

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.wppl$" . js2-mode))
  :config
  (setq js2-basic-offset 2)

  (define-key js2-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key js2-mode-map (kbd "M-p") 'flycheck-previous-error))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))


(use-package input
  :bind (("C-\\" . toggle-input-method)))

;; (use-package image+
;;   :ensure t
;;   :config (imagex-global-sticky-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (concat live-etc-dir "multiple-cursors-prefs.el"))

  (use-package phi-search
    :ensure t
    :config
    (global-set-key (kbd "C-s") 'phi-search)
    (global-set-key (kbd "C-r") 'phi-search-backward))

  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-M-z" . mc/edit-lines)))

;; (use-package ace-jump-mode
;;   :ensure t
;;   :bind (("C-o" . ace-jump-mode)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :pin melpa-stable
  :config
  (setq magit-refresh-status-buffer nil)
  (setq vc-handled-backends nil)
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
 (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
 (global-flycheck-mode))

(use-package paredit
  :ensure t)

;; (use-package puppet-mode
;;   :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  ;; (markdown-reload-extensions)
  (setq markdown-command "multimarkdown")
  (setq markdown-enable-math t))

;; (use-package writeroom-mode
;;   :ensure t)

(use-package erlang
  :ensure t
  :mode (("\\.erl\\'" . erlang-mode))
  :config
  (defun my/erlang-mode-hook ()
    ;; (setq erlang-electric-commands '(erlang-electric-comma erlang-electric-semicolon))
    ;; (setq erlang-electric-newline-inhibit-list '(erlang-electric-gt))
    ;; (setq erlang-electric-newline-inhibit t)
    )
  (add-hook 'erlang-mode-hook 'my/erlang-mode-hook)

  (define-key erlang-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key erlang-mode-map (kbd "M-p") 'flycheck-previous-error))

(use-package elpy
  :ensure t
  :pin elpy
  :config
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-flymake
                       elpy-module-highlight-indentation
                       elpy-module-pyvenv
                       ;;elpy-module-yasnippet
                       ;;elpy-module-django
                       ))

  (elpy-enable)
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setq python-shell-completion-native nil)
  (elpy-use-ipython)
  (define-key python-mode-map (kbd "C-c C-l") 'elpy-shell-send-region-or-buffer)
  (define-key python-mode-map (kbd "C-c C-;") 'elpy-shell-send-current-statement)
  (define-key python-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key python-mode-map (kbd "M-p") 'flycheck-previous-error))

;; (use-package ein
;;   ;; https://github.com/millejoh/emacs-ipython-notebook
;;   :ensure t)

;; (use-package idris-mode
;;   :ensure t)

;; (use-package ess
;;   :ensure t
;;   :pin melpa-stable)

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

  (custom-set-variables
     '(TeX-source-correlate-method 'synctex)
     '(TeX-source-correlate-mode t)
     '(TeX-source-correlate-start-server t))

  ;; (TeX-view)
  (setq TeX-command-force "LatexMk")
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b -g %n %o %b")))
  (add-hook 'LaTeX-mode-hook
          (lambda ()
             (add-hook 'after-save-hook (lambda () (TeX-command-sequence t t)) 'append 'local))))

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
  :bind (("C-c C-k" . recompile)
         ("C-c C-l" . kill-compilation))
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
  (define-key c-mode-base-map (kbd "RET") 'electric-newline-and-maybe-indent)
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

;; (use-package swift-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'flycheck-checkers 'swift))

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
  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  (epa-file-name-regexp-update)
  ;(setq epa-armor t)
  )

;; (use-package winner
;;   :bind
;;   (("C-c b" . winner-undo)
;;    ("C-c f" . winner-redo)))

(use-package dired
  :config
  (setq dired-dwim-target t))

;; (use-package go-mode
;;   :ensure t)

;; (use-package fstar-mode
;;   :ensure t)

;; (use-package fsharp-mode
;;   :mode "\\.fs[iylx]?$"
;;   :ensure t)

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

(comment
 (use-package spacemacs-theme
   :ensure t))

;;(cycle-my-theme)
(comment
 (use-package solarized-theme
   :ensure t
   :if window-system
   :init
   (setq solarized-use-variable-pitch nil
         solarized-use-more-italic t
         solarized-emphasize-indicators nil
         solarized-distinct-fringe-background nil
         solarized-high-contrast-mode-line t
         solarized-scale-org-headlines nil
         )
   :config
   (load "solarized-theme-autoloads" nil t)
   (load-theme 'solarized-light t))
 )
(comment
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
  (setq projectile-switch-project-action 'projectile-dired))

(define-key comint-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(use-package helm
  :ensure t
  :diminish helm-mode
  :config (setq helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match    t
                helm-split-window-default-side 'other)

  (setf helm-boring-buffer-regexp-list '("\\` " "\\*helm" "\\*helm-mode"
                                       "\\*Echo Area" "\\*Minibuf" "\\*monky-cmd-process\\*"
                                       "\\*epc con" "\\*Compile-Log\\*" "\\*monky-process\\*"
                                       "\\*CEDET CScope\\*" "\\*Messages\\*" "\\*Flycheck error"
                                       "\\*.+(.+)" "elpa/.+" "tramp/.+"
                                       "\\*Gofmt Errors\\*" "\\*autopep8"
                                       "\\*magit-process:" "\\*magit-diff:" "\\*anaconda-mode\\*"
                                       "\\*ESS"
                                       "....Output" ;; TeX Output? wtf
                                       "\\*Preview-Ghostscript"
                                       "*_region_.tex"
                                       ".+el.gz"))

  (setf helm-white-buffer-regexp-list '("\\*helm ag results\\*"))

  (defun my-filter-dired-buffers (buffer-list)
    (delq nil (mapcar
               (lambda (buffer)
                 (if (memq (with-current-buffer buffer major-mode)
                           (list 'debugger-mode 'special-mode 'help-mode))
                     nil
                   buffer))
               buffer-list)))

  (advice-add 'helm-skip-boring-buffers :filter-return 'my-filter-dired-buffers)

  (global-set-key (kbd "C-x k") 'my-kill-this-buffer)

  (defun my-kill-this-buffer ()
    (interactive)
    (let ((projectile-require-project-root nil))
      (if (projectile-project-p)
          (let*
              ((buffers (mapcar (lambda (x) (format "%s" x)) (projectile-project-buffers)))
               ;;(buffers (funcall (helm-attr 'buffer-list helm-source-buffers-list)))
               (candidates (cdr (helm-skip-boring-buffers buffers 'nil)))
               (root (projectile-project-root))
               (next (car candidates)))
            (unless next
                    (message "Last buffer for project %s" root))
            (kill-this-buffer)
            (if next
                (switch-to-buffer next)
                (dired root)))
          (kill-this-buffer))))

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

          ;; (use-package helm-c-yasnippet
          ;;   :ensure t
          ;;   :config
          ;;   (setq helm-yas-space-match-any-greedy t)
          ;;   (global-set-key (kbd "C-c y") 'helm-yas-complete))

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

            ;; (use-package nameframe-projectile
            ;;   :ensure t
            ;;   :config (nameframe-projectile-mode t))

            :bind (("C-x f" . helm-for-files)
                   ("M-s a" . helm-projectile-ag)
                   ("M-s p" . helm-projectile-ag)
                   ("M-s A" . projectile-ag)
                   ;;("C-c p p" . helm-projectile-switch-project)
                   ("C-x C-M-f" . helm-projectile-find-file-dwim)))

          (use-package helm-ag
            :ensure t
            :bind (("C-c a" . helm-projectile-ag)))

          (use-package helm-descbinds
            :ensure t
            :config (helm-descbinds-mode)))

  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x j" . helm-imenu)
         ("C-x p" . helm-etags-select)
         ("M-y" . helm-show-kill-ring)
         ("M-X" . helm-resume)
         ;;("M-l" . helm-projectile)
         ("M-l" . helm-mini)
         ("M-o" . helm-projectile)
         ;;("M-O" . helm-resume)
         ;;("C-h SPC" . helm-all-mark-rings)
         ))

(global-set-key (kbd "<C-tab>")     'helm-mini)

(use-package browse-at-remote
  :ensure t
  :bind (("C-c g g" . browse-at-remote)))

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
  (let ((current (selected-window))
        (new-f (make-frame)))
    (set-frame-parameter new-f 'width 110)
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
(global-set-key (kbd "C-x 5 6") 'my/toggle-window-split)

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
;;(global-set-key  (kbd "M-U") (lambda () (interactive) (forward-line -10)))
;;(global-set-key  (kbd "M-D") (lambda () (interactive) (forward-line 10)))
;;(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
;;(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-s")   'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-,")   'pop-tag-mark)

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

(defun detexify ()
  (interactive)
  (dired-smart-shell-command "open -a Detexify" nil nil))

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
  :ensure t
  :config
  (add-to-list 'clojure-mode-hook 'paredit-mode))

(use-package helm-cider
  :ensure t
  :config (helm-cider-mode 1))

(use-package dumb-jump
  :ensure t
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljc"))
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs"))
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "clj"))
  (add-to-list 'dumb-jump-language-file-exts '(:language "erlang" :ext "erl"))
  (add-to-list 'dumb-jump-find-rules
               '(:type "function" :language "erlang" :regex "^JJJ("))

  ;; (add-to-list 'dumb-jump-language-file-exts '(:language "haskell" :ext "hs"))
  ;; (add-to-list 'dumb-jump-find-rules
  ;;              '(:type "function" :language "haskell" :regex "^\\s+JJJ\\j"))
  (dumb-jump-mode))
(dumb-jump-mode nil)

(use-package sml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode)))

;; (use-package raml-mode
;;   ;:ensure t
;;   :init)

(comment
 (set-fringe-mode
  (/ (- (frame-pixel-width)
        (* 100 (frame-char-width)))
     2))
 (set-fringe-mode nil)
)

(defun my/center (width)
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode)))

(use-package unipoint
  :ensure t
  :bind
  (("C-'" . unipoint-insert)
   ("C-c '" . unipoint-insert)))

(use-package char-menu
  :ensure t
  :bind (("C-c C-'" . char-menu))
  :config
  (setq char-menu
        '("≈"
          "Σ" "Π" "Λ"
          "⊢" "⊨"
          "≡"
          ;; "≠" "∞" "×" "±" "√"
          ;; "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓"
          "α" "β"
          ;; "Y" "δ"
          "ε" ;; "ζ"
          "η" ;;"θ" "ι" "κ"
          "λ" "μ"
          ;; "ν" "ξ" "ο" "π" "ρ"
          "σ" "τ" "υ" "φ" "χ"
          "ψ" "ω")))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

;; (my/center 120)

;; (use-package cider-eval-sexp-fu
;;   :pin melpa-stable
;;   :ensure t)

;; (package-refresh-packages)


(defun gorilla-empty-all-results* ()
  (while (re-search-forward
          ";; @@\n\\(;; ->\n\\)?\\(;;;.*\n\\)*\\(;; <-\n\\)?;; =>\n\\(;;;.*\n\\)+;; <="
          nil t)
    (replace-match ";; @@")))

(defun gorilla-empty-all-results ()
  "Empty all Gorilla REPL result blocks"
  (interactive)
  (save-excursion
    (goto-char 0)
    (gorilla-empty-all-results*)))

;; http://blog.brunobonacci.com/2016/03/18/emacs-incanter-hack/
(setq filechart-temp-file "/tmp/chart.png")
(setq filechart-wait-time 500)

(defun filechart-display-image-inline (buffer-name file-name)
  "Use `BUFFER-NAME' to display the image in `FILE-NAME'.
  Checks weather `BUFFER-NAME' already exists, and if not create
  as needed."
  (switch-to-buffer-other-window buffer-name)
  (iimage-mode t)
  (read-only-mode -1)
  (kill-region (point-min) (point-max))
  ;; unless we clear the cache, the same cached image will
  ;; always get re-displayed.
  (clear-image-cache nil)
  (insert-image (create-image file-name))
  (read-only-mode t))

(defun filechart-eval-and-display ()
  "Evaluate the expression preceding point
   and display the chart into a popup buffer"
  (interactive)
  (let ((old-buf (current-buffer)))
    (condition-case nil
                    (delete-file filechart-temp-file)
                    (error nil))
    (cider-eval-last-sexp)
    (sleep-for 0 filechart-wait-time)
    (filechart-display-image-inline "*filechart-chart*" filechart-temp-chart-file)
    (switch-to-buffer-other-window old-buf)))

;; (use-package smartscan
;;   :ensure t
;;   :config
;;   ;; conflicts with flycheck M-n M-p
;;   ;; try smartscan-or-flycheck?
;;   (add-to-list 'prog-mode-hook 'smartscan-mode))

;;; local stuff

(use-package proof-site
  :defer t
  :mode ("\\.v\\'" . coq-mode)
  :load-path
  "~/.emacs.d/vendor/PG/generic"
  :config
  (use-package company-coq
    :ensure t
    :config
    (add-hook 'coq-mode-hook #'company-coq-mode)
    (add-to-list 'flycheck-disabled-checkers 'coq))
  :bind
  ("C-c C-'" . proof-assert-until-point-interactive)
  ("M-r" . proof-goto-point))

;;(load "/Users/vladki/.opam/4.02.0/share/emacs/site-lisp/tuareg-site-file")
;;(setq twelf-root "/Users/vladki/src/oplss/twelf/")
;;(load (concat twelf-root "emacs/twelf-init.el"))
;;(add-to-list 'load-path "~/.emacs.d/vendor/parinfer-mode")
;;(require 'parinfer-mode)

(use-package dash :ensure t)
(use-package dash-functional :ensure t)
(use-package fill-column-indicator :ensure t)
(use-package f :ensure t)
(use-package s :ensure t)

;;(setq lean-rootdir "/usr/local")
;;(setq-local lean-emacs-path "/usr/local/share/emacs/site-lisp/lean")
;;(add-to-list 'load-path (expand-file-name lean-emacs-path))
;;(require 'lean-mode)

(comment
(js2r-add-keybindings-with-prefix "C-c C-m")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fancy haskell modules https://github.com/chrisdone/chrisdone-emacs/blob/6b1ea1ff0a3931a0921861390565562bf1779548/config/haskell.el#L481
;; uses completing-read

(setq haskell-import-mapping
      '(("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)
")
        ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT
")
        ("Data.ByteString" . "import qualified Data.ByteString as S
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Char8" . "import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L
")
        ("Data.ByteString.Lazy.Char8" . "import qualified Data.ByteString.Lazy.Char8 as L8
")
        ("Data.Map" . "import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
")
        ("Data.Map.Strict" . "import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
")
        ("Data.Set" . "import qualified Data.Set as S
import Data.Set (Set)
")
        ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)
")
        ("Data.Vector.Storable" . "import qualified Data.Vector.Storable as SV
import Data.Vector (Vector)
")
        ("Data.Conduit.List" . "import qualified Data.Conduit.List as CL
")
        ("Data.Conduit.Binary" . "import qualified Data.Conduit.Binary as CB
")))


(defun haskell-capitalize-module (m)
  ;; FIXME:
  (with-temp-buffer
    (insert m)
    (upcase-initials-region (point-min) (point-max))
    (buffer-string)))

(defvar haskell-fast-module-list
  (list)
  "A list of modules.")

(defun haskell-fast-modules-save ()
  (interactive)
  (with-current-buffer (find-file-noselect "~/.emacs.d/.haskell-modules.el")
    (erase-buffer)
    (insert (format "%S" haskell-fast-module-list))
    (basic-save-buffer)
    (bury-buffer)))

(defun haskell-fast-modules-load ()
  (interactive)
  (with-current-buffer (find-file-noselect "~/.emacs.d/.haskell-modules.el")
    (setq haskell-fast-module-list (read (buffer-string)))
    (bury-buffer)))


(defun haskell-fast-get-import (custom)
  (if custom
      (let* ((module (haskell-capitalize-module (read-from-minibuffer "Module: " ""))))
        (unless (member module haskell-fast-module-list)
          (add-to-list 'haskell-fast-module-list module))
        module)
    (let ((module (haskell-capitalize-module
                   (completing-read
                    "Module: "
                    (append (mapcar #'car haskell-import-mapping)
                            haskell-fast-module-list) nil nil))))
      (unless (member module haskell-fast-module-list)
        (add-to-list 'haskell-fast-module-list module)
        (haskell-fast-modules-save))
      module)))

(defun haskell-fast-add-import (custom)
  "Add an import to the import list.  Sorts and aligns imports,
unless `haskell-stylish-on-save' is set, in which case we defer
to stylish-haskell."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (haskell-navigate-imports)
    (let* ((chosen (haskell-fast-get-import custom))
           (module (let ((mapping (assoc chosen haskell-import-mapping)))
                     (if mapping
                         (cdr mapping)
                       (concat "import " chosen "\n")))))
      (insert module))
    (haskell-sort-imports)
    (haskell-align-imports)))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :commands haskell-mode
  :bind (("M-," . pop-global-mark))
  :config
  (add-to-list 'load-path "~/.emacs.d/vendor/intero/elisp")
  (require 'intero)
  (add-hook 'haskell-mode-hook 'intero-mode)
  ;; (custom-set-variables
  ;;  '(haskell-process-path-ghci "env")
  ;;  '(haskell-process-type (quote ghci))
  ;;  '(haskell-process-suggest-haskell-docs-imports nil)
  ;;  '(haskell-process-args-ghci
  ;;    (quote
  ;;     ("DYLD_INSERT_LIBRARIES=/System/Library/Frameworks/GLUT.framework/GLUT"
  ;;      "stack" "ghci" "--ghci-options" "-fno-ghci-sandbox"))))

  ;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  ;(define-key haskell-mode-map (kbd "C-i") 'haskell-fast-add-import)
  (define-key haskell-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key haskell-mode-map (kbd "M-p") 'flycheck-previous-error)
  (define-key haskell-mode-map (kbd "M-r") 'my-intero-repl-load-nopop)
  (add-hook 'haskell-mode-hook
          (lambda ()
             (add-hook 'after-save-hook 'my-intero-repl-load-nopop nil t))))

(defun my-intero-repl-load-nopop ()
  (interactive)
  (let ((buf (current-buffer)))
        (intero-repl-load)
        (pop-to-buffer buf)))

;;;; useful buffers (taken from spacemacs)

;; Regexp for useful and useless buffers for smarter buffer switching

(defvar useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")

(defvar useful-buffers-regexp '("\\*\\(scratch\\|terminal\.\+\\|ansi-term\\|eshell\\)\\*")
  "Regexp used to define buffers that are useful despite matching `useless-buffers-regexp'.")



(defun useless-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-paren-major-mode (get (with-current-buffer buffer
                                     major-mode)
                                   'derived-mode-parent))
        (buf-name (buffer-name buffer)))
    ;; first find if useful buffer exists, if so returns nil and don't check for
    ;; useless buffers. If no useful buffer is found, check for useless buffers.
    (unless (cl-loop for regexp in useful-buffers-regexp do
                     (when (or (eq buf-paren-major-mode 'comint-mode)
                               (string-match regexp buf-name))
                       (return t)))
      (cl-loop for regexp in useless-buffers-regexp do
               (when (string-match regexp buf-name)
                 (return t))))))


(defun next-useful-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (next-buffer)
    (while (and (useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (next-buffer))))

(defun previous-useful-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (previous-buffer)
    (while (and (useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (previous-buffer))))


(global-set-key  (kbd "C-x <left>") 'previous-useful-buffer)
(global-set-key  (kbd "C-x <right>") 'next-useful-buffer)
(global-set-key  (kbd "C-x C-b") 'ibuffer)

(global-set-key  (kbd "M-r") 'recompile)


(define-key mac-apple-event-map [core-event open-documents] 'my-mac-ae-open-documents)

(defun my-mac-ae-open-documents (event)
  "Open the documents specified by the Apple event EVENT."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (dolist (file-name (mac-ae-list ae nil 'undecoded-file-name))
      (if file-name
          (dnd-open-local-file
           (concat "file://"
                   (mapconcat 'url-hexify-string
                              (split-string file-name "/") "/")) nil)))
    (let ((selection-range (mac-ae-selection-range ae))
          (search-text (mac-ae-text-for-search ae)))
      (cond (selection-range
             (let ((line (car selection-range))
                   (start (cadr selection-range))
                   (end (nth 2 selection-range)))
               (if (>= line 0)
                   (progn
                     (goto-char (point-min))
                     (forward-line line)) ; (1- (1+ line))
                 (if (and (>= start 0) (>= end 0))
                     (progn (set-mark (1+ start))
                            (goto-char (1+ end)))))))
            ((stringp search-text)
             (re-search-forward
              (mapconcat 'regexp-quote (split-string search-text) "\\|")
              nil t))))
    (mac-odb-setup-buffer ae))
  (select-frame-set-input-focus (make-frame)))

;; (use-package elm-mode
;;   :ensure t)

(use-package highlight-sexp
  :ensure t
  :diminish highlight-sexp-mode
  :config
  (global-highlight-sexp-mode))

;; (use-package octave
;;   :ensure t
;;   :mode (("\\.m$" . octave-mode)))

(eval-after-load
 'octave
 '(defun inferior-octave-output-digest (_proc string)
    "Special output filter for the inferior Octave process.
Save all output between newlines into `inferior-octave-output-list', and
the rest to `inferior-octave-output-string'."
    (setq string (concat inferior-octave-output-string string))
    (while (string-match "\n" string)
           (setq inferior-octave-output-list
                 (append inferior-octave-output-list
                         (list (substring string 0 (match-beginning 0))))
                 string (substring string (match-end 0))))
    (setq inferior-octave-receive-in-progress nil)   ;;; XXX THIS
    (if (string-match inferior-octave-prompt string)
        (setq inferior-octave-receive-in-progress nil))
    (setq inferior-octave-output-string string)))


(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
        (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; (stop-using-minibuffer)
