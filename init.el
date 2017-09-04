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


;; Split window on startup
(split-window-horizontally)

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
(hc-toggle-highlight-trailing-whitespace 1)


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
 '(custom-safe-themes
   (quote
    ("cd0d4fdf764f757fd659ee2697239a62f38d15203000ced1ad8e43c978942c68" default)))
 '(package-selected-packages
   (quote
    (railscasts-reloaded-theme sparql-mode vcl-mode dockerfile-mode google-this git-gutter magit enh-ruby-mode projectile better-defaults auto-dim-other-buffers rspec-mode rubocop company counsel ivy ruby-block ruby-additional robe relative-line-numbers multiple-cursors material-theme highlight-chars helm haml-mode git-commit diff-hl cl-lib-highlight bundler auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#263238" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "nil" :family "Hack"))))
 '(auto-dim-other-buffers-face ((t (:background "gray10"))))
 '(hc-trailing-whitespace ((t (:background "gray57"))))
 '(mode-line-buffer-id ((t (:foreground "green2" :weight bold)))))

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

;; Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "gcc " (buffer-name) " && ./a.out" ))
  (shell-command foo))
(global-set-key (kbd "C-§") 'execute-c-program)

;; Projectile
(projectile-mode 1)

;; Projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; enh-ruby-mode
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

;; Backups and Autosave file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; git-gutter
(global-git-gutter-mode +1)

;; google-this
(google-this-mode 1)

;; Dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Varnish
(add-to-list 'auto-mode-alist '("*.vcl" . vcl-mode))

;; Robe
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; Kill buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; mutli-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(setq multi-term-program-switches "--login")

;; Display time
(display-time-mode 1)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Weather (wttrin)
(setq wttrin-default-cities '("London" "England"))

;; Desktop save
(desktop-save-mode 1)

;; Custom keys minor mode
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Duplicate line
    (define-key map (kbd "C-c C-d") "\C-a\C- \C-n\M-w\C-y")
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

;; auto-complete
(ac-config-default)
(global-auto-complete-mode t)
