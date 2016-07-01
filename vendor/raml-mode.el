;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Resource Aware ML
;;
;; Emacs Mode
;;
;; Author: Jan Hoffmann
;;
;; Year: 2011,2012
;;



(provide 'raml-mode)

(add-to-list 'auto-mode-alist '("\\.raml\\'" . raml-mode))


(setq path-to-raml "$HOME/src/oplss/raml-1.1/main")
(setq raml-output-buffer "*RAML Output*")
(setq raml-process-name "RAML")

(setq raml-verbose nil)
(setq raml-time nil)
(setq raml-keep-temp-files nil)
(setq raml-max-degree 2)


(setq raml-keywords
 '())

;; define a var for your keymap, so that you can set it as local map
;; (meaning, active only when your mode is on)
(defvar raml-mode-map nil "Keymap for raml-mode")

;; assign your keyboard shortcuts
(setq raml-mode-map (make-sparse-keymap))
(define-key raml-mode-map (kbd "C-c C-c") 'raml-evaluate)
(define-key raml-mode-map (kbd "C-c C-h") 'raml-analyse-heap)
(define-key raml-mode-map (kbd "C-c C-e") 'raml-analyse-eval)
(define-key raml-mode-map (kbd "C-c C-t") 'raml-analyse-ticks)
(define-key raml-mode-map (kbd "C-c C-o") 'raml-analyse-overflow)
(define-key raml-mode-map (kbd "C-c C-v") 'raml-set-verbose)
(define-key raml-mode-map (kbd "C-c C-S-k") 'raml-set-keep-temp-files)
(define-key raml-mode-map (kbd "C-c C-S-t") 'raml-set-time)
(define-key raml-mode-map (kbd "C-c C-d") 'raml-set-max-degree)
(define-key raml-mode-map (kbd "C-c C-k") 'raml-kill-run)


;; the command to comment/uncomment text
(defun raml-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "(*") (comment-end "*)"))
     (comment-dwim arg)))


(define-derived-mode raml-mode fundamental-mode
  (setq font-lock-defaults '(raml-keywords))

  ;; modify the keymap
  (define-key raml-mode-map [remap comment-dwim] 'raml-comment-dwim)

  (setq mode-name "RAML")

  (use-local-map raml-mode-map)

  ;; Mathematica style comment: “(* ... *)”
  (modify-syntax-entry ?\( "() 1" raml-mode-syntax-table)
  (modify-syntax-entry ?\) ")( 4" raml-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" raml-mode-syntax-table)
)




(defun raml-set-max-degree ()
  "Set the maximal degree."
  (interactive)
  (let* (
   (degree (read-number "Maximal degree: " 2))
   (msg (concat "The maximal degree is now " (number-to-string degree) ".") )
  )
  (setq raml-max-degree degree)
  (message msg)
  )
)





(defun raml-set-keep-temp-files ()
  "Toggle verbose output."
(interactive)
(if raml-keep-temp-files
 (progn (setq raml-keep-temp-files nil)
        (message "RAML is now deleting temporary files.") )
 (setq raml-keep-temp-files t)
 (message "RAML is now keeping temporary files.")
)
)


(defun raml-set-verbose ()
  "Toggle verbose output."
(interactive)
(if raml-verbose
 (progn (setq raml-verbose nil)
        (message "RAML is now non-verbose.") )
 (setq raml-verbose t)
 (message "RAML is now verbose.")
)
)

(defun raml-set-time ()
  "Toggle time measurement."
(interactive)
(if raml-time
 (progn (setq raml-time nil)
        (message "RAML run time counter off.") )
 (setq raml-time t)
 (message "RAML run time counter on.")
))

(defun raml-options ()
  (let* ((time (if raml-time " --time" ""))
         (verbose (if raml-verbose " --verbose" ""))
         (keepfiles (if raml-keep-temp-files " --keepfiles" ""))
        )
  (concat time verbose keepfiles)
 ))


(defun raml-analyse-heap ()
  "Analyse the current file."
(interactive)
  (let* (
    (degree (number-to-string raml-max-degree))
    (raml-action (concat "analyze heap " degree) )
    (file-name (buffer-file-name))
    (cmd-str (concat path-to-raml " " raml-action " " file-name " " (raml-options)))
   )
   (raml-run cmd-str)
))

(defun raml-analyse-eval ()
  "Analyse the current file."
(interactive)
  (let* (
    (degree (number-to-string raml-max-degree))
    (raml-action (concat "analyze steps " degree) )
    (file-name (buffer-file-name))
    (cmd-str (concat path-to-raml " " raml-action " " file-name " " (raml-options)))
   )
   (raml-run cmd-str)
))


(defun raml-analyse-ticks ()
  "Analyse the current file."
(interactive)
  (let* (
    (degree (number-to-string raml-max-degree))
    (raml-action (concat "analyze ticks " degree) )
    (file-name (buffer-file-name))
    (cmd-str (concat path-to-raml " " raml-action " " file-name " " (raml-options)))
   )
   (raml-run cmd-str)
))


(defun raml-run (cmd-stre)
    (start-process-shell-command raml-process-name raml-output-buffer cmd-str)
;;     (shell-command cmd-str raml-output-buffer)
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer raml-output-buffer)
    (clear-buffer)
    (other-window 1)
)



(defun raml-analyse-overflow ()
  "Analyse the current file."
(interactive)
  (let* (
    (degree (number-to-string raml-max-degree))
    (raml-action (concat "analyze overflow " degree) )
    (file-name (buffer-file-name))
    (cmd-str (concat path-to-raml " " raml-action " " file-name " " (raml-options)))
   )
   (raml-run cmd-str)
))



(defun raml-evaluate ()
  "Evaluate the current file."
(interactive)
  (let* (
    (raml-action (concat "eval") )
    (file-name (buffer-file-name))
    (cmd-str (concat path-to-raml " " raml-action " " file-name " " (raml-options)))
   )
   (raml-run cmd-str)
))


(defun clear-buffer()
 "Kill all of the text in the current buffer."
 (interactive)
 (delete-region 1 (point-max))
 (beginning-of-buffer)
)


(defun raml-kill-run ()
 "Kill the running raml execution."
 (interactive)
 (kill-process raml-process-name)
)
