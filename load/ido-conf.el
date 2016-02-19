(require 'recentf)


(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (setq ido-ignore-buffers (list "\\` "))
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (setq ido-enable-prefix nil
        ido-create-new-buffer 'always
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-everywhere 1
        ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys))

;(use-package flx
;  :ensure t)

(use-package flx-ido
  :ensure t
  :config (flx-ido-mode 1))

;; (use-package idomenu
;;   :ensure t
;;   :bind (("C-x C-i" . idomenu)))

(icomplete-mode 1)

(defvar live-symbol-names)
(defvar live-name-and-pos)

(setq recentf-max-saved-items 100)
(recentf-mode t)

(defvar ido-dont-ignore-buffer-names '())

(defun ido-ignore-most-star-buffers (name)
  (and
   (string-match-p "^*" name)
   (not (member name ido-dont-ignore-buffer-names))))
