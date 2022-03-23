(defun tc/make-pretty ()
  (interactive)
  (when (locate-dominating-file (expand-file-name default-directory)
                                "prettier.config.js")
    (shell-command (format "yarn prettier --write %s" buffer-file-name))
    (revert-buffer nil t)))

(defun tc/turn-on-prettier-on-save ()
  (add-hook 'after-save-hook 'tc/make-pretty nil 'local))

(use-package! js2-mode
  :init
  (add-hook! js2-mode 'tc/turn-on-prettier-on-save))
