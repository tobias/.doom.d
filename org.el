(after! org (setq org-tags-column -78))

(require 'org-weekpage)

(setq weekpage-path "~/Dropbox/journal/")

(define-key weekpage-mode-map (kbd "<C-left>") 'weekpage-prev)
(define-key weekpage-mode-map (kbd "<C-right>") 'weekpage-next)
;; override compile
(define-key weekpage-mode-map (kbd "C-c c") 'org-ctrl-c-ctrl-c)
(global-unset-key "\C-co")
(global-set-key "\C-con" 'this-weeks-weekpage)
(global-set-key "\C-coN" 'find-weekpage)

(setq shortcut-link-regex
  "^https://app[^.]*\\.\\(clubhouse\\.io\\|shortcut\\.com\\)/[^/]+/\\(story\\|epic\\)/\\([0-9]+\\)")

(defun tc/org-insert-link (url description)
  (insert "[[" url "][" description "]]"))

(defun org-insert-shortcut-link (url)
  (interactive (list (let ((url (current-kill 0 t)))
                       (if (string-match shortcut-link-regex url)
                           url
                         (read-string "Shortcut URL: ")))))
  (when (null (string-match shortcut-link-regex url))
    (error "Invalid Shortcut URL '%s'" url))
  (let ((shortcut-id (match-string 3 url)))
    (tc/org-insert-link url (concat "sc-" shortcut-id))))
