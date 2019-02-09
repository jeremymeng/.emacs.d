;; -*- mode: emacs-lisp -*-
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; allow M-SPC as a prefix
(global-set-key (kbd "M-SPC") nil)

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
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind ("M-SPC g s" . magit-status)
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

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package company
  :defer t
  :ensure t
  :hook (prog-mode . company-mode))

(use-package eldoc
  :ensure t
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

(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  ("M-SPC s s" . swiper)
  ("M-SPC s r" . counsel-rg)
  ("M-x" . counsel-M-x)
  ("C-x b" . ivy-switch-buffer)
  ("M-SPC b b" . ivy-switch-buffer)
  :diminish counsel-mode)
(counsel-mode t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package tide
  :ensure t
  :init
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2))
  (setq typescript-indent-level 2)
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package js2-mode
  :ensure t)
(setq js-indent-level 2)

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

(defalias 'yes-or-no-p 'y-or-n-p)
(defun kill-this-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))
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

(fset 'kill-and-close-frame
      (lambda ()
        (interactive)
                            (kill-buffer)
                            (delete-frame)))
(global-set-key [C-f4]    'kill-and-close-frame)

(global-set-key (kbd "M-o")     'other-window)
(global-set-key [C-f4]    'kill-this-buffer)
(global-set-key (kbd "<f8>") 'isearch-backward-symbol-at-point)

(prefer-coding-system 'utf-8)

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
    (wgrep golden-ratio flycheck-typescript-tslint js2-mode olivetti tide counsel company markdown-mode helm gh git-timemachine fullframe magit undo-tree)))
 '(show-paren-mode t nil (paren))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((((class color) (min-colors 16) (background dark)) (:foreground "SteelBlue"))))
 '(font-lock-comment-face ((((class color) (min-colors 16) (background dark)) (:foreground "SteelBlue")))))
