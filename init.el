;;; init.el --- emacs configuration -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;; Set default font
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq user-emacs-directory "~/.tryout/")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))

(setq gc-cons-threshold 50000000)

;; allow M-SPC as a prefix
(global-set-key (kbd "M-SPC") nil)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :bind
  (("C-c j" . undo-tree-undo)
   ("C-c k" . undo-tree-redo)
   ("C-c l" . undo-tree-switch-branch)
   ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package magit
  :defer t
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind
  ("M-SPC g s" . magit-status)
  :commands magit-status)

(use-package forge
   :ensure t)

(use-package fullframe
  :ensure t)

(fullframe magit-status magit-mode-quit-window)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package gh
  :defer t
  :ensure t)

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :ensure t)

(use-package company
  :defer t
  :ensure t
  :hook (prog-mode . company-mode))

(use-package eldoc
  :ensure t
  :commands eldoc-mode)

(use-package lisp-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

(use-package hideshow
  :bind
  ("C-c h" . hs-toggle-hiding))

(use-package files
  :config
  (setq backup-directory-alist `(("." . "~/.saves"))
        version-control t
        kept-new-versions 10
        kept-old-versions 0
        delete-old-versions t
        backup-by-copying t))

(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-formats
        '((mark modified read-only " " (name 16 16) " "
                (size 9 -1 :right) " " (mode 16 16 :left :elide)
                " " (process 8 -1) " " filename)
          (mark " " (name 16 -1) " " filename))
        ;; ibuffer-elide-long-columns t
        ibuffer-eliding-string "&"))

(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line) ; Make highlight extend all the way to the right
  ;; TODO testing out the fuzzy search
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
                  (t . ivy--regex-plus)))
  (setq enable-recursive-minibuffers t)
  :bind
  ("M-SPC s s" . swiper)
  ("M-SPC s r" . counsel-rg)
  ("M-SPC f f" . counsel-file-jump)
  ("M-x" . counsel-M-x)
  ("C-x b" . ivy-switch-buffer)
  ("M-SPC b b" . ivy-switch-buffer)
  ("<f1> f" . 'counsel-describe-function)
  ("<f1> v" . 'counsel-describe-variable)
  ("<f1> l" . 'counsel-find-library)
  ("<f2> i" . 'counsel-info-lookup-symbol)
  ("<f2> u" . 'counsel-unicode-char)
  :diminish (counsel-mode . ""))
(counsel-mode t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package tide
  :ensure t
  :init
  (setq tide-format-options
        '(:tabSize 2
                   :convertTabsToSpaces t
                   :insertSpaceAfterFunctionKeywordForAnonymousFunctions nil
                   :placeOpenBraceOnNewLineForFunctions nil
                   :indentSize 2
                   :insertSpaceBeforeFunctionParenthesis nil
                   ))
  (setq typescript-indent-level 2)
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;(before-save . tide-format-before-save)
         ))

(use-package js2-mode
  :ensure t)
(setq js-indent-level 2)

;; js repl/debugger. needs `npm i -g indium`
(use-package indium
  :ensure t
  :defer t)

(use-package golden-ratio
  :ensure t)

;; required for editing in the ivy-occur buffer
(use-package wgrep
  :ensure t)

;; distraction free mode
(use-package olivetti
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  :diminish yas-minor-mode)

(use-package rg
  :commands (rg rg-project rg-dwim)
  :ensure t)

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("M-SPC p" . projectile-command-map)
  :config
  (projectile-mode 1)
  :ensure t)

(use-package smex
  :ensure t)

(use-package expand-region
  :bind
  ("M-SPC v" . er/expand-region)
  :config
  (setq expand-region-contract-fast-key "V")
  :ensure t)

(use-package which-key
  :config
  (which-key-mode 1)
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :diminish smartparens-mode)

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 120
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package move-text
  :ensure t
  :bind
  (([(meta up)] . move-text-up)
   ([(meta down)] . move-text-down)))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package multiple-cursors
  :ensure t
  :bind (
         ("M-SPC s m a" . mc/mark-all-dwim)
         ("M-SPC s m b" . mc/mark-all-like-this)
         ("M-SPC s m m" . mc/mark-more-like-this-extended)
         ("M-SPC s m r" . mc/edit-lines)
         ("M-SPC s m s l" . mc/insert-letters)
         ("M-SPC s m s m" . mc/mark-sgml-tag-pair)
         ("M-SPC s m s n" . mc/insert-numbers)
         ("M-SPC s m s r" . set-rectangular-region-anchor)
         ("M-SPC s m s s" . mc/sort-regions)
         ("M-SPC s m s t" . mc/reverse-regions)
         ;; ("M-." . mc/mark-next-like-this)
         ;; ("M-," . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(server-start)

(load-theme 'leuven 'no-confirm)

;; Misc. customization
(setq require-final-newline t)
(setq inhibit-startup-message t)
(line-number-mode t)
(column-number-mode t)
(setq-default transient-mark-mode t)
(setq fill-column 80)
(setq-default indent-tabs-mode nil)
(setq frame-title-format
      '("%S: " (buffer-file-name "%f"
                                 (dired-directory dired-directory "%b"))))
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

(global-auto-revert-mode t)

(defun my/text-mode-setup ()
  "Configure text mode hooks."
  (olivetti-mode 1)
  (olivetti-set-width 140)
  (flyspell-mode 1)
  (visual-line-mode 1))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (my/text-mode-setup))))

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

(defalias 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1)

;;
;; Set key macros
;;
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

(defun kill-this-buffer ()
  "Kill current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

(fset 'kill-and-close-frame
      (lambda ()
        (interactive)
                            (kill-buffer)
                            (delete-frame)))
(global-set-key [C-f4]    'kill-and-close-frame)
(global-set-key (kbd "M-SPC b d")    'kill-this-buffer)

(global-set-key (kbd "M-o")     'other-window)
(global-set-key (kbd "<f8>") 'isearch-backward-symbol-at-point)

(prefer-coding-system 'utf-8)

;; Prefer vertical split
(setq split-height-threshold 200)

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
    (smex rg projectile indium wgrep golden-ratio flycheck-typescript-tslint js2-mode olivetti tide counsel company markdown-mode helm gh git-timemachine fullframe magit undo-tree)))
 '(show-paren-mode t nil (paren))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((((class color) (min-colors 16) (background dark)) (:foreground "SteelBlue"))))
 '(font-lock-comment-face ((((class color) (min-colors 16) (background dark)) (:foreground "SteelBlue")))))

;;; init.el ends here
