;; highlight expression on eval

(defun live-bounds-of-preceding-sexp ()
  "Return the bounds of sexp before the point. Copies semantics
   directly from the fn preceding-sexp to ensure highlighted area
   is identical to that which is evaluated."
  (let ((opoint (point))
	ignore-quotes
	expr)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
	;; If this sexp appears to be enclosed in `...'
	;; then ignore the surrounding quotes.
	(setq ignore-quotes
	      (or (eq (following-char) ?\')
		  (eq (preceding-char) ?\')))
	(forward-sexp -1)
	;; If we were after `?\e' (or similar case),
	;; use the whole thing, not just the `e'.
	(when (eq (preceding-char) ?\\)
	  (forward-char -1)
	  (when (eq (preceding-char) ??)
	    (forward-char -1)))

	;; Skip over hash table read syntax.
	(and (> (point) (1+ (point-min)))
	     (looking-back "#s" (- (point) 2))
	     (forward-char -2))

	;; Skip over `#N='s.
	(when (eq (preceding-char) ?=)
	  (let (labeled-p)
	    (save-excursion
	      (skip-chars-backward "0-9#=")
	      (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
	    (when labeled-p
	      (forward-sexp -1))))

	(save-restriction
	  ;; vladimir@cs.ualberta.ca 30-Jul-1997: skip ` in
	  ;; `variable' so that the value is returned, not the
	  ;; name
	  (if (and ignore-quotes
		   (eq (following-char) ?`))
	      (forward-char))
	  (cons (point) opoint))))))

(defun live-bounds-of-defun ()
  "Return the bounds of the defun around point. Copies semantics
   directly from the fn eval-defun-2 to ensure highlighted area
   is identical to that which is evaluated."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (setq beg (point))
    (read (current-buffer))
    (setq end (point))
    (cons beg end)))

;; fix up esf to highlight exactly what emacs evaluates
(defun live-esf-initialize-elisp ()
  (define-eval-sexp-fu-flash-command eval-last-sexp
    (eval-sexp-fu-flash (when (ignore-errors (preceding-sexp))
                          (with-esf-end-of-sexp
                            (live-bounds-of-preceding-sexp)))))
  (define-eval-sexp-fu-flash-command eval-defun
    (eval-sexp-fu-flash (live-bounds-of-defun))))

(use-package highlight
  :demand
  :ensure t
  :init (progn
          (use-package eval-sexp-fu
            :ensure t
            :demand
            :config
            (setq eval-sexp-fu-flash-duration 0.5)
            (live-esf-initialize-elisp))))
