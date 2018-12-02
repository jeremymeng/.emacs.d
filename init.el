;; -*- mode: lisp -*-
;;
;; Set whatever
;;

(add-to-list 'load-path "~/.emacs.d/elisp")

(display-time)
(setq make-backup-files nil)
;; Automaticly delete the auto save file (usually #*#)
(setq delete-auto-save-files t)
;; Always add a final new line to the end of each file
(setq require-final-newline t)
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)
(line-number-mode t)
(column-number-mode t)
(setq-default transient-mark-mode t)
(setq fill-column 80)
(auto-image-file-mode t)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq java-mode-hook '(lambda () (setq c-basic-offset 2)))
(setq frame-title-format
      '("%S: " (buffer-file-name "%f"
                                 (dired-directory dired-directory "%b"))))
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))


(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (auto-fill-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; hippie-expand
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(add-hook 'sgml-mode-hook 'font-lock-turn-off-thing-lock)

;; fullscreen frames
; (add-to-list 'default-frame-alist '(maximize-emacs-window))

;; (server-start)
;; ;; gnuserv
;; (require 'gnuserv)
;; (gnuserv-start)
;; (setq gnuserv-frame (selected-frame))
;; (setenv "GNUSERV_SHOW_EMACS" "1")

(defalias 'yes-or-no-p 'y-or-n-p)
(defun kill-this-buffer () 
    (interactive) 
    (kill-buffer (current-buffer)))

(defun maximize-emacs-window ()
  "Maximizing window."
  (interactive)
  (w32-send-sys-command #xf030)
)
(defun restore-emacs-window ()
  "Restoring window."
  (interactive)
  (w32-send-sys-command 61728)
)

;;
;; Set key macros
;;
(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key "\C-c\C-l" 'what-line)
(global-set-key [delete]   'delete-char)
(global-set-key [home]     'beginning-of-line)
(global-set-key [end]      'end-of-line)
(global-set-key [prior]    'scroll-down)
(global-set-key [next]     'scroll-up)
(global-set-key [C-right]  'forward-word)
(global-set-key [C-left]   'backward-word)
(global-set-key [C-home]  'beginning-of-buffer)
(global-set-key [C-end]   'end-of-buffer)
(global-set-key [C-f5]    'compile)

(fset 'kill-and-close-frame
      (lambda ()
        (interactive)
                            (kill-buffer)
                            (delete-frame)))
(global-set-key [C-f4]    'kill-and-close-frame)

(global-set-key (kbd "M-o")     'other-window)
(global-set-key [f10]     'magit-status)
(global-set-key [f3]      'grep)
(global-set-key [f12]     'maximize-emacs-window)
(global-set-key [C-f12]   'restore-emacs-window)
(global-set-key [C-f4]    'kill-this-buffer)
(global-set-key (kbd "<f8>") 'isearch-backward-symbol-at-point)

;; initial package setup
;(push "path/to/use-package" load-path)
(require 'use-package)
(require 'package)
(mapc (lambda(p) (push p package-archives))
      '(
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-refresh-contents)
(package-initialize)

;; a lot of use-package stuffs are from https://github.com/jordonbiondo/.emacs.d/blob/master/init.el

;; this will install undo-tree if it's not there
;; and it will set it up how I want
(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package magit
  :defer t
  :ensure t
  :bind ("C-x m" . magit-status)
  :commands magit-status)

(use-package fullframe
  :ensure t)

(fullframe magit-status magit-mode-quit-window)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package gh
  :defer t
  :ensure t)

(use-package helm
  :defer t
  :ensure t)

(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config (progn
            (defun web-indirect-this-thing()
              (interactive)
              (let ((beg 0) (end 0))
                (save-excursion
                  (setq beg (progn (web-mode-forward-sexp -1)
                                   (call-interactively 'web-mode-tag-end)
                                   (point)))
                  (setq end (progn  (web-mode-forward-sexp 1)
                                    (point))))
                (indirect-region beg end))))
  :ensure t)

(require 'cl)
(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode)
  :config (progn
            (add-hook 'csharp-mode-hook
                      (defun jorbi/csharp-setup-function ()
                        (setq c-basic-offset 4
                              indent-tabs-mode nil)
                        (hs-minor-mode t)
                        ))
            (font-lock-add-keywords
             'csharp-mode
             '(("\\(// *\\)\\(todo\\)\\(.*$\\)" 2 'font-lock-warning-face t))))
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package company
  :defer t
  :ensure t)

(use-package eldoc
  :commands eldoc-mode)

(use-package lisp-mode
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding))

(use-package files
  :config (setq backup-directory-alist `(("." . "~/.saves"))
                version-control t
                kept-new-versions 10
                kept-old-versions 0
                delete-old-versions t
                backup-by-copying t))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config (setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
                              (size 9 -1 :right) " " (mode 16 16)
                              " " (process 8 -1) " " filename)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&"))

(use-package paredit
  :defer t
  :ensure t)

(use-package clojure-mode
  :defer t
  :ensure t
  :config (progn
            (add-hook 'clojure-mode-hook 'enable-paredit-mode)
            ))

(use-package clojure-mode-extra-font-locking
  :defer t
  :ensure t)

(use-package cider
  :defer t
  :ensure t
  :config (progn
            (add-hook 'cider-mode-hook 'eldoc-mode)))

(use-package rainbow-delimiters
  :defer t
  :ensure t)

(load-theme 'leuven 'no-confirm)

;; set by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(display-time-mode t)
 '(flyspell-default-dictionary "english")
 '(font-latex-do-multi-line t)
 '(global-font-lock-mode t nil (font-lock))
 '(package-selected-packages
   (quote
    (rainbow-delimiters cider clojure-mode-extra-font-locking clojure-mode paredit company markdown-mode csharp-mode web-mode helm gh git-timemachine fullframe magit undo-tree)))
 '(show-paren-mode t nil (paren))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((((class color) (min-colors 16) (background dark)) (:foreground "SteelBlue"))))
 '(font-lock-comment-face ((((class color) (min-colors 16) (background dark)) (:foreground "SteelBlue")))))
