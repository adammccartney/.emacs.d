;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Use this at the top of your .emacs file for local overrides:
;;     (let ((init "~/.emacs.d/init.elc"))
;;       (if (file-exists-p init)
;;           (load-file init)
;;         (load-file (substring init 0 -1))))

;;; Code:

(make-directory (locate-user-emacs-file "local") :no-error)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s" emacs-version))
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s/lisp" emacs-version))
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s/etc" emacs-version))


(add-to-list 'load-path 
             (format "~/.emacs.d/site-lisp/%s/dash" emacs-version))
(add-to-list 'load-path 
             (format "~/.emacs.d/site-lisp/%s/transient/lisp" emacs-version))
(add-to-list 'load-path 
             (format "~/.emacs.d/site-lisp/%s/with-editor" emacs-version))
(add-to-list 'load-path 
             (format "~/.emacs.d/site-list/%s/magit/lisp" emacs-version))
;(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               (format "~/.emacs.d/site-lisp/%s/magit/Documentation/" emacs-version)))


;; Package bootstrap
(load-file "~/.emacs.d/packages.el")
(require 'autoloads)
(setf package-enable-at-startup nil)
(require 'use-package)

;; "Local" packages
(require 'unannoy)
(require 'ctags)
(require 'smtpmail)


(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Some global keybindings
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(column-number-mode)
(global-display-line-numbers-mode t)

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))

;; Frames and fonts

(defvar my-preferred-fonts
  '("Noto Mono-10"
    "Inconsolata-12"))

(defun my-set-preferred-font (&optional frame)
  "Set the first available font from `my-preferred-fonts'."
  (catch 'done
    (with-selected-frame (or frame (selected-frame))
      (dolist (font my-preferred-fonts)
        (when (ignore-errors (x-list-fonts font))
          (set-frame-font font)
          (throw 'done nil))))))

(defun my-set-frame-fullscreen (&optional frame)
  (set-frame-parameter frame 'fullscreen 'fullheight))

(add-hook 'after-make-frame-functions #'my-set-preferred-font)
(add-hook 'after-make-frame-functions #'my-set-frame-fullscreen t)


;; Calendar and planner notification stuff
(appt-activate t)

;;; Individual package configurations

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))

(use-package dired
  :defer t
  :config
  (progn
    (add-hook 'dired-mode-hook #'toggle-truncate-lines)
    (setf dired-listing-switches "-alhG"
          dired-guess-shell-alist-user
          '(("\\.pdf\\'" "evince")
            ("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")
            ("\\(\\.png\\|\\.jpe?g\\)\\'" "qiv")
            ("\\.gif\\'" "animate")))))

(use-package lisp-mode
  :defer t
  :config
  (progn
    (defun ert-all ()
      (interactive)
      (ert t))
    (defun ielm-repl ()
      (interactive)
      (pop-to-buffer (get-buffer-create "*ielm*"))
      (ielm))
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
    (defalias 'lisp-interaction-mode 'emacs-lisp-mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package time
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format t)
    (display-time-mode t)))

(use-package diff-mode
  :defer t
  :config (add-hook 'diff-mode-hook #'read-only-mode))

(use-package color-theme-sanityinc-tomorrow
  :config
  (setf custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-night)
  (global-hl-line-mode 1)
  (custom-set-faces
   '(cursor ((t :background "#eebb28")))))

(use-package uniquify
  :defer t
  :config
  (setf uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package winner
  :config
  (winner-mode 1)
  (windmove-default-keybindings))

(use-package calc
  :defer t
  :config (setf calc-display-trail nil))

(use-package eshell
  :defer t
  :bind ([f1] . eshell-as)
  :init
  (setf eshell-directory-name (locate-user-emacs-file "local/eshell"))
  :config
  (add-hook 'eshell-mode-hook ; Bad, eshell, bad!
            (lambda ()
              (define-key eshell-mode-map (kbd "<f1>") #'quit-window))))

(use-package markdown-mode
  :defer t
  :mode ("\\.md$" "\\.markdown$" "vimperator-.+\\.tmp$")
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (remove-hook 'fill-nobreak-predicate
                           'markdown-inside-link-p t)))
  (setf sentence-end-double-space nil
        markdown-indent-on-enter nil
        markdown-command
        "pandoc -f markdown -t html5 -s --self-contained --smart"))

(use-package octave
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  (setf octave-block-offset 4))

(use-package cc-mode
  :defer t
  :init
  (defun skeeto/c-hook ()
    (setf c-basic-offset 4)
    (c-set-offset 'case-label '+)
    (c-set-offset 'access-label '/)
    (c-set-offset 'label '/))
  :config
  (progn
    (define-key java-mode-map (kbd "C-x I") 'add-java-import)
    (add-hook 'c-mode-hook #'skeeto/c-hook)
    (add-hook 'c++-mode-hook #'skeeto/c-hook)
    (add-to-list 'c-default-style '(c-mode . "k&r"))
    (add-to-list 'c-default-style '(c++-mode . "k&r"))))

(use-package ctags
  :init
  (global-set-key (kbd "M-.") #'ctags-find)
  (global-set-key (kbd "M-?") #'ctags-find-reference))

(use-package nasm-mode
  :defer t
  :mode ("\\.nasm$" "\\.asm$" "\\.s$")
  :config
  (add-hook 'nasm-mode-hook (lambda () (setf indent-tabs-mode t))))

(use-package asm-mode
  :defer t
  :init
  (add-hook 'asm-mode-hook (lambda () (setf indent-tabs-mode t
                                            tab-always-indent t))))

(use-package x86-lookup
  :defer t
  :bind ("C-h x" . x86-lookup)
  :functions x86-lookup-browse-pdf-evince
  :config
  (let ((pdf-regexp ".*sdm-vol-2abcd\\.pdf$")
        (pdf-dir "~/doc/"))
    (setf x86-lookup-browse-pdf-function #'x86-lookup-browse-pdf-evince
          x86-lookup-pdf (ignore-errors
                           (car (directory-files pdf-dir t pdf-regexp))))))

(use-package ielm
  :defer t
  :config
  (define-key ielm-map (kbd "C-c C-z") #'quit-window))

(use-package paren
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")
  (setf rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4"))


(use-package javadoc-lookup
  :defer t
  :bind ("C-h j" . javadoc-lookup)
  :config
  (ignore-errors
    (setf javadoc-lookup-cache-dir (locate-user-emacs-file "local/javadoc"))))

(use-package browse-url
  :defer t
  :init
  (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "firefox")
    (setf browse-url-browser-function #'browse-url-firefox)))

(use-package uuid-simple
  :demand t
  :bind ("C-x !" . uuid-insert)
  :config (random (make-uuid)))

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map (kbd "f") #'push-first-button))

(use-package slime
  :init 
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(use-package org 
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (setq org-startup-folded "showall"))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))


(use-package mu4e
  :defer 20
  :config
  
  ;; automatically add a gpg signature to every email (signed as adam@mur.at) 
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")

  (setq mu4e-attachment-dir
    (lambda (fname mtype)
      (cond 
        ;; docfiles go to ~/Documents
        ((and fname (string-match "\\.doc$" fname)) "~/Documents")
        ((and fname (string-match "\\.pdf$" fname)) "~/Documents")
        (t "~/Downloads")))) ;; everything else


  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-contexts
          `(,(make-mu4e-context
              :name "murat"
              :match-func (lambda (msg) (when msg
                                          (string-prefix-p "/murat" (mu4e-message-field msg :maildir))))
              :vars '(
                      (user-full-name . "Adam McCartney")
                      (user-mail-address . "adam@mur.at")
                      (smtpmail-smtp-server . "smtp.mur.at")
                      (smtpmail-smtp-service . 465)
                      (smtpmail-stream-type . starttls)
                      (mu4e-sent-folder . "/murat/Sent")
                      (mu4e-trash-folder . "/murat/Trash")
                      (mu4e-drafts-folder . "/murat/Drafts")
                      (mu4e-refile-folder . "/murat/Archive")
                      (mu4e-sent-messages-behavior . sent)
                      (mu4e-compose-signature .
                                              (concat
                                                "Adam McCartney | https://admccartney.mur.at \n"
                                                "Markhofgasse 11-17/2/6 1030 Vienna\n"))))
            ,(make-mu4e-context
              :name "work-mdw"
              :match-func (lambda (msg) (when msg
                                          (string-prefix-p "/mdw" (mu4e-message-field msg :maildir))))
              :vars '(
                      (user-full-name . "Adam McCartney")
                      (user-mail-address . "mccartney@mdw.ac.at")
                      (smtpmail-smtp-server . "mail.mdw.ac.at")
                      (smtpmail-smtp-service . 587)
                      (smtpmail-stream-type . ssl)
                      (mu4e-send-folder . "/mdw/Sent Items")
                      (mu4e-trash-folder . "/mdw/Trash")
                      (mu4e-drafts-folder . "/mdw/Work In Progress")
                      (mu4e-refile-folder . "/mdw/Cabinet")
                      (mu4e-sent-message-behavior . sent)
                      (mu4e-compose-signature .
                                              (concat
                                                "Adam McCartney\n"
                                                "Software Developer\n"
                                                "Zentraler Informatik Dienst (ZID)\n"
                                                "mdw - Universitaet fuer Musik und darstellende Kunst Wien\n"
                                                "Anton-von-Webern-Platz 1, 1030 Wien\n"
                                                "+43 1 71155 7333\n"))))
          ))
  (setq mu4e-context-policy 'pick-first)
  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Use mu4e for sending e-mail
  (setq message-send-mail-function 'smtpmail-send-it)

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "All Inboxes"
                :query "maildir:/murat/INBOX"
                :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; allow for flowed formatting
  (setq mu4e-compose-format-flowed t)

  ;; Use a specific key for signing by referencing its thumbprint
  (setq mml-secure-openpgp-signers '("C5BF27EE0290CDE5BC8A8801A5FCE0B0A42EFDA8"))

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

 (use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;;(when (file-directory-p "~/.local/src")
    ;;(setq projectile-project-search-path '("~/.local/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;;(use-package lsp-treemacs
;;  :after lsp)

(use-package lsp-ivy)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
 (setq which-key-idle-delay 1))

;;(use-package lsp-pyright
;;  :ensure t 
;;  :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp)))) ;; or lsp-deferred

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :config
  (setq python-indent-level 4))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(provide 'init) ; make (require 'init) happy
