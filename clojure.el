;; use word wrapping instead of truncation in test report and doc
;; buffers. Must be set before loading cider.
(setq cider-special-mode-truncate-lines nil)

(setq shortcut-elisp-loaded nil)
(when (file-exists-p "~/.doom.d/shortcut-shared.el")
  (load! "shortcut-shared.el"))

(when shortcut-elisp-loaded
  (shortcut-backend-enable-matcher-combinator-test-output-colors)
  (add-hook 'clojure-mode-hook 'shortcut-backend-font-lock))

(defun tc/rename-buffer-to-ns ()
  (interactive)
  (when (buffer-file-name)
    (let ((ns (clojure-expected-ns)))
      (when (and (stringp ns)
                 (not (string= "" ns)))
        (rename-buffer ns)))))

(use-package! clojure-mode
  :init
  (add-hook! clojure-mode 'tc/turn-on-paredit)
  (add-hook! clojure-mode 'tc/rename-buffer-to-ns)
  (add-hook! clojure-mode 'subword-mode)
  (lambda ()
    (load! "lib/cljstyle-mode")
    (when shortcut-elisp-loaded
      (add-hook! clojure-mode-hook 'shortcut-backend-font-lock)))
  (setq clojure-indent-style :always-align)

  :bind
  (:map clojure-mode-map
   ;; unset toggling keyword to string and vice-versa since it conflicts with my
   ;; ace-window binding
   ("C-:" . nil)
   ;; run cljstyle on file. I don't use cljstyle-mode to do this
   ;; automatically on save since it causes the buffer to jump and breaks
   ;; compilation buffer references in the file
   ("C-c TAB" . 'cljstyle)
   ("C-c d" . 'tc/insert-divider-comment)
   ("C-c C-n f" . 'tc/insert-fixme)
   ("C-c C-n t" . 'tc/insert-todo)
   ("C-c C-n c" . 'tc/insert-nocommit)
   ("C-c C-n g" . 'tc/insert-given)
   ("C-c C-n w" . 'tc/insert-when)
   ("C-c C-n n" . 'tc/insert-then)
   ("C-c C-n a" . 'tc/insert-and)
   ("M-c" . 'tc/insert-spy)))

(use-package! cider
  :init
  (add-hook! cider-repl-mode 'tc/turn-on-paredit)
  (setq
   cider-font-lock-dynamically        nil
   cider-auto-select-error-buffer     t
   ;; try symbol at point before asking
   cider-prompt-for-symbol            nil
   cider-eval-spinner-type            'vertical-breathing
   cider-repl-print-length            100
   cider-repl-wrap-history            t
   cider-repl-history-file           (concat user-emacs-directory "cider-repl-history")
   cider-repl-pop-to-buffer-on-connect 'display-only
   cider-repl-use-content-types       nil
   cider-repl-display-help-banner     nil
   cider-repl-prompt-function         (lambda (namespace)
                                        (format "%s>\n" namespace)))

  :bind
  (:map cider-repl-mode-map
   ("RET" . 'indent-new-comment-line)
   ("C-RET" . 'cider-repl-return)
   ("C-c RET" . 'cider-repl-return)
   ("M-c" . 'tc/insert-spy-letsc)))

(when shortcut-elisp-loaded
  ;; use my local cider alias and set up scope-capture
  (setq shortcut-backend-default-clojure-cli-options
      "-J-server -J-Xmx8g -J-XX:+UseG1GC -J-Dapple.awt.UIElement=true -J-Dtika.config=tika-config.xml -A:backend-defaults:dev:test")
  (add-to-list 'cider-jack-in-nrepl-middlewares "sc.nrepl.middleware/wrap-letsc")
  (add-to-list 'cider-jack-in-nrepl-middlewares "refactor-nrepl.middleware/wrap-refactor")
  (cider-add-to-alist 'cider-jack-in-dependencies "vvvvalvalval/scope-capture-nrepl" "0.3.1"))

(defun tc/insert-spy ()
  (interactive)
  (move-beginning-of-line nil)
  (cider-interactive-eval "(require 'sc.api)")
  (insert "(sc.api/spy)")
  (indent-for-tab-command))

(defun tc/insert-spy-letsc ()
  (interactive)
  (insert "(sc.api/letsc )")
  (backward-char))

(defun tc/insert-divider-comment ()
  (interactive)
  (move-beginning-of-line nil)
  (insert ";; -----  -----\n")
  (previous-line)
  (search-forward "- "))

(defun tc/insert-comment (text)
  (move-beginning-of-line nil)
  (insert (format ";; %s" text))
  (indent-for-tab-command))

(defun tc/insert-note (type)
  (tc/insert-comment (format "%s: (toby) " type)))

(defun tc/insert-fixme ()
  (interactive)
  (tc/insert-note "FIXME"))

(defun tc/insert-todo ()
  (interactive)
  (tc/insert-note "TODO"))

(defun tc/insert-nocommit ()
  (interactive)
  (tc/insert-note "NOCOMMIT"))

(defun tc/insert-given ()
  (interactive)
  (tc/insert-comment "Given: "))

(defun tc/insert-when ()
  (interactive)
  (tc/insert-comment "When: "))

(defun tc/insert-then ()
  (interactive)
  (tc/insert-comment "Then: "))

(defun tc/insert-and ()
  (interactive)
  (tc/insert-comment "And: "))
