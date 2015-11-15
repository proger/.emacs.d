(use-package nix-mode
  :ensure t)

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


; (require 'flycheck)
; (add-hook 'after-init-hook #'global-flycheck-mode)
; 
; (setq flycheck-check-syntax-automatically '(save))
; 
; (flycheck-define-checker nix
;   "Nix checker"
;   :command ("nix-instantiate"
;             "--parse-only"
;             source)
;   :error-patterns
;   ((error line-start "error:" (message) ", at " (file-name) ":" line ":" column line-end))
;   :modes nix-mode)
; 
; (add-to-list 'flycheck-checkers 'nix)
