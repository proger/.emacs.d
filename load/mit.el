(add-to-list 'load-path "~/.emacs.d/vendor/6.945-config")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         MIT-scheme config                        ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the place where you have installed scheme. Be sure to set
;; this to an appropriate value!!!
;; http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm
(setq scheme-root "/usr/local/scmutils")

(setq scheme-program-name
      (concat
       scheme-root "/mit-scheme/bin/mit-scheme "
       "--library " scheme-root "/mit-scheme/lib "
       "--band " "edwin-mechanics.com"
       " -heap 10000"))

;; Use the Edwin-like MIT/Scheme interpreter:
(load "xscheme")

;; generic scheme completion
(require 'scheme-complete)
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(setq lisp-indent-function 'scheme-smart-indent-function)

;; mit-scheme documentation
(require 'mit-scheme-doc)

;; Special keys in scheme mode. Use <tab> to indent scheme code to the
;; proper level, and use M-. to view mit-scheme-documentation for any
;; symbol.
(eval-after-load
 'scheme
 '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'xscheme
 '(define-key scheme-interaction-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'scheme
 '(define-key scheme-mode-map (kbd "M-.") 'mit-scheme-doc-lookup))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map (kbd "M-.")
    'mit-scheme-doc-lookup))

(eval-after-load
 'xscheme
 '(define-key scheme-interaction-mode-map (kbd "M-.")
    'mit-scheme-doc-lookup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Print a Buffer to PDF  (C-c C-p)         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq pdf-target-name (concat "/tmp/" (buffer-name) ".pdf"))
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " "\"" pdf-target-name "\""))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " pdf-target-name)))

;;(global-set-key (kbd "C-c C-p") 'print-to-pdf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Patch for xscheme - Fixing evaluate-expression in debugger ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xscheme-prompt-for-expression-exit ()
  (interactive)
  (let (
        ;; In Emacs 21+, during a minibuffer read the minibuffer
        ;; contains the prompt as buffer text and that text is
        ;; read only.  So we can no longer assume that (point-min)
        ;; is where the user-entered text starts and we must avoid
        ;; modifying that prompt text.  The value we want instead
        ;; of (point-min) is (minibuffer-prompt-end).
        (point-min (if (fboundp 'minibuffer-prompt-end)
                              (minibuffer-prompt-end)
                            (point-min))))
    (if (eq (xscheme-region-expression-p point-min (point-max)) 'one)
        (exit-minibuffer)
      (error "input must be a single, complete expression"))))
