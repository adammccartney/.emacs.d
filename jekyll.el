;;; jekyll.el --- Emacs Jekyll integration

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; These are some helper functions to make using Jekyll easier. When
;; you use this, make sure you set `jekyll-home'.

;; The relevant user functions are:
;;   * `jekyll/new-post' -- Create a new blog post

;; More functions will be added in the future as I think of them. I do
;; all my publishing through Git, for which I use Magit. That means I
;; don't intend on creating any publish/deploy functions.

;;; Code:

(defvar jekyll-home "."
  "The root directory for your Jekyll blog.")

(defvar jekyll-post-layout "post"
  "Default layout for blog posts.")

(defun jekyll/new-post (title)
  "Start a new Jekyll blog post."
  (interactive "MTitle: ")
  (find-file (jekyll/get-post-file title))
  (insert "---\n")
  (insert (format "title: %s\n" title))
  (insert (format "layout: %s\n" jekyll-post-layout))
  (insert "---\n\n"))

(defun jekyll/get-post-file (title)
  "Return the filename for a new post given the TITLE."
  (expand-file-name (format "%s/_posts/%s-%s.markdown"
                            jekyll-home
                            (format-time-string "%Y-%m-%d")
                            (replace-regexp-in-string "\\W+" "-" title))))

(provide 'jekyll)

;;; jekyll.el ends here
