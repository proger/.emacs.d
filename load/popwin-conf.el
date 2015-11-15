(use-package popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer)

  (setq popwin:special-display-config
        '(("*Help*"  :height 30)
          ("*Completions*" :noselect t)
          ("*Messages*" :noselect t :height 30)
          ("*Apropos*" :noselect t :height 30)
          ("*compilation*" :noselect t)
          ("*Backtrace*" :height 30)
          ("*Messages*" :height 30)
          ("*Occur*" :noselect t)
          ("*Ido Completions*" :noselect t :height 30)
          ("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
          ("*magit-diff*" :noselect t :height 40 :width 80)
          ("*magit-edit-log*" :noselect t :height 15 :width 80)
          ("\\*ansi-term\\*.*" :regexp t :height 30)
          ("*shell*" :height 30)
          (".*overtone.log" :regexp t :height 30)
          ("*gists*" :height 30)
          ("*sldb.*":regexp t :height 30)
          ("*cider-error*" :height 30 :stick t)
          ("*cider-doc*" :height 30 :stick t)
          ("*cider-src*" :height 30 :stick t)
          ("*cider-result*" :height 30 :stick t)
          ("*cider-macroexpansion*" :height 30 :stick t)
          ("*Kill Ring*" :height 30)
          ("*Compile-Log*" :height 30 :stick t)
          ("*git-gutter:diff*" :height 30 :stick t))))

(defun live-show-messages ()
  (interactive)
  (popwin:display-buffer "*Messages*"))

(defun live-display-messages ()
  (interactive)
  (popwin:display-buffer "*Messages*"))

(defun live-display-ansi ()
  (interactive)
  (popwin:display-buffer "*ansi-term*"))