;;; functions.el -*- lexical-binding: t; -*-

(defun tc/turn-on-paredit ()
  (paredit-mode t)
  (when (not (display-graphic-p))
    (define-key paredit-mode-map (kbd "M-[ c") 'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-[ d") 'paredit-forward-barf-sexp)))
