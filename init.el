(require 'package)

;; Melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib

  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Marmalade
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; Theme
(load-theme 'material t)

;; Add line numbers
(global-linum-mode t)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; multiline-cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-h") 'set-rectangular-region-anchor)

;; Whitespace
(hc-toggle-highlight-trailing-whitespace)

;; Font size
(set-face-attribute 'default nil :height 100)

;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-dim-other-buffers rspec-mode rubocop company counsel ivy ruby-block ruby-additional robe relative-line-numbers multiple-cursors material-theme highlight-chars helm haml-mode git-commit diff-hl cl-lib-highlight bundler auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Bracket Highlighting
(show-paren-mode 1)

;; Code Completion


;; Rubocop
(require 'rubocop)


;; Rspec Mode
(require 'rspec-mode)

;; Auto dim when lost focus
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))
