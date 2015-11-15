(require 'recentf)

(use-package ido
  :ensure t
  :bind (("C-x f" . ido-recentf-open))
  :config
  (ido-mode t)
  (setq ido-ignore-buffers (list "\\` "))
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (setq ido-enable-prefix nil
        ido-create-new-buffer 'always
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-everywhere 1))

;(use-package flx
;  :ensure t)

(use-package flx-ido
  :ensure t
  :config (flx-ido-mode 1))

(use-package idomenu
  :ensure t
  :bind (("C-x C-i" . idomenu)))

(icomplete-mode 1)

(defvar live-symbol-names)
(defvar live-name-and-pos)

(setq recentf-max-saved-items 25)
(recentf-mode t)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if
      (find-file (ido-completing-read "Find recent file: "
                                      (mapcar (lambda (path)
                                                (replace-regexp-in-string (expand-file-name (getenv "HOME")) "~" path))
                                              recentf-list) nil t))
      (message "Opening file...")
    (message "Aborting")))

(defvar ido-dont-ignore-buffer-names '())

(defun ido-ignore-most-star-buffers (name)
  (and
   (string-match-p "^*" name)
   (not (member name ido-dont-ignore-buffer-names))))

