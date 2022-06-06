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




;; Package bootstrap
(load-file "~/.emacs.d/packages.el")
(require 'autoloads)
(setf package-enable-at-startup nil)
(require 'use-package)

;; "Local" packages
(require 'unannoy)
(require 'extras)
(require 'ctags)
(require 'smtpmail)

;; Some global keybindings
(global-set-key (kbd "C-x k") #'kill-this-buffer)

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

;;; Individual package configurations

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))

(use-package impatient-mode
  :defer t
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters))

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

(use-package undo-tree
  :init
  (setf undo-tree-mode-lighter ""))

(use-package evil
  :init
  (setf evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (defvar my-leader-map
    (let ((map (make-sparse-keymap)))
      (prog1 map
        (define-key map "w" 'elfeed))))
  (define-key evil-normal-state-map "\\" my-leader-map)
  (define-key evil-normal-state-map (kbd "M-.") #'ctags-find)
  (define-key evil-normal-state-map (kbd "M-?") #'ctags-find-reference)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (add-to-list 'evil-emacs-state-modes 'special-mode)
  (add-to-list 'evil-emacs-state-modes 'youtube-dl-list-mode)
  (add-to-list 'evil-emacs-state-modes 'process-menu-mode)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setf evil-ex-search-highlight-all nil)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
  (add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w"))))

(use-package evil-smartparens
  :defer t
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  :config
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil)
  (sp-use-paredit-bindings))

(use-package time
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format t)
    (display-time-mode t)))

(use-package comint
  :defer t
  :config
  (progn
    (define-key comint-mode-map (kbd "<down>") #'comint-next-input)
    (define-key comint-mode-map (kbd "<up>") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-n") #'comint-next-input)
    (define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-r") #'comint-history-isearch-backward)
    (setf comint-prompt-read-only t
          comint-history-isearch t)))

(use-package tramp
  :defer t
  :config
  (setf tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

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

(use-package simple
  :defer t
  :config
  (progn
    ;; disable so I don't use it by accident
    (define-key visual-line-mode-map (kbd "M-q") (expose (lambda ())))
    (add-hook 'tabulated-list-mode-hook #'hl-line-mode)))

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

(use-package gitconfig-mode
  :defer t
  :config (add-hook 'gitconfig-mode-hook
                    (lambda ()
                      (setf indent-tabs-mode nil
                            tab-width 4))))

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

(use-package simple-httpd
  :defer t
  :functions httpd-send-header
  :config
  (defservlet uptime "text/plain" ()
    (princ (emacs-uptime)))
  (defun httpd-here ()
    (interactive)
    (setf httpd-root default-directory))
  (defadvice httpd-start (after httpd-query-on-exit-flag activate)
    (let ((httpd-process (get-process "httpd")))
      (when httpd-process
        (set-process-query-on-exit-flag httpd-process nil))))
  (setf httpd-host "0.0.0.0"))

(use-package js2-mode
  :defer t
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
  (setf js2-skip-preprocessor-directives t)
  (setq-default js2-additional-externs
                '("$" "unsafeWindow" "localStorage" "jQuery"
                  "setTimeout" "setInterval" "location" "skewer"
                  "console" "phantom")))

(use-package skewer-mode
  :defer t
  :init
  (skewer-setup))

(use-package skewer-repl
  :defer t
  :config (define-key skewer-repl-mode-map (kbd "C-c C-z") #'quit-window))

(use-package ps-print
  :defer t
  :config (setf ps-print-header nil))

(use-package erc
  :defer t
  :config
  (when (eq 0 (string-match "wello" (user-login-name)))
    (setf erc-nick "skeeto")))

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

(use-package icomplete
  :init
  (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<C-tab>" . minibuffer-force-complete)))

(use-package etags
  :defer t
  :config
  (defun etags-build (directory)
    (interactive "DDirectory: ")
    (let* ((results ())
           (head (list directory))
           (tail head))
      (while head
        (dolist (file (directory-files (car head) t nil t))
          (cond ((and (not (string-match "\\.$" file))
                      (not (string-match "\\.\\.$" file))
                      (file-directory-p file))
                 (let ((new-tail (list file)))
                   (setf (cdr tail) new-tail
                         tail new-tail)))
                ((string-match "\\.[ch]$" file)
                 (push file results))))
        (pop head))
      (let ((default-directory directory))
        (apply #'call-process "etags" nil nil nil results)))))

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

(use-package compile-bind
  :demand t
  :bind (("C-h g" . compile-bind-set-command)
         ("C-h G" . compile-bind-set-root-file))
  :config
  (progn
    (setf compilation-always-kill t
          compilation-ask-about-save nil
          compilation-scroll-output 'first-error
          compile-bind-command (format "make -kj%d " (numcores)))
    (when (executable-find "nmake.exe")
      (compile-bind-set-command "nmake -nologo "))
    (compile-bind* (current-global-map)
                   ("C-x c" ""
                    "C-x t" 'test
                    "C-x C" 'clean))))

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map (kbd "f") #'push-first-button))

(use-package gamegrid
  :defer t
  :init
  (setf gamegrid-user-score-file-directory (locate-user-emacs-file "games")))

(use-package ospl-mode
  :defer t
  :init
  (autoload 'ospl-mode "ospl-mode"))

(use-package sql
  :defer t
  :init
  (setf sql-product 'sqlite))

(use-package enriched
  :defer t
  :config
  (define-key enriched-mode-map "\C-m" nil))

(use-package slime
  :init 
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(use-package org 
  :init
  (setq org-startup-folded "showall"))

(use-package mu4e
  :defer 20
  :config
  
  ;; automatically add a gpg signature to every email (signed as adam@mur.at) 
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")

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
                      (smtpmail-smtp-service . 465)
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
  (when (file-directory-p "~/.local/src")
    (setq projectile-project-search-path '("~/.local/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))



(provide 'init) ; make (require 'init) happy

