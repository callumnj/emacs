(server-start)

(require 'package)

; Add downloaded packages to load path
(add-to-list 'load-path "~/.emacs.d/my_packages/")
(require 'find-file-in-project)

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

;; SSH aliases
(add-to-list 'load-path "~/.emacs.d/ssh.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(beacon-color "#e8b81a")
 '(beacon-mode t)
 '(column-number-mode t)
 '(csv-separators (quote ("," ";")))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "9fe1540491fcf692b8c639a3abacd32b29233bc4cb834a12a0fd1e01cbd0a128" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "cd0d4fdf764f757fd659ee2697239a62f38d15203000ced1ad8e43c978942c68" default)))
 '(default-input-method "japanese")
 '(disable-mouse-global-mode nil nil (disable-mouse))
 '(display-time-default-load-average nil)
 '(display-time-load-average-threshold 0.1)
 '(display-time-mode t)
 '(doom-modeline-bar-width 3)
 '(doom-modeline-buffer-encoding nil)
 '(doom-modeline-mode t)
 '(electric-pair-mode t)
 '(fast-but-imprecise-scrolling nil)
 '(fci-rule-color "#37474f")
 '(global-whitespace-mode nil)
 '(hl-sexp-background-color "#1c1f26")
 '(indent-tabs-mode nil)
 '(initial-scratch-message "Hello Callum!!!
")
 '(js-indent-level 2)
 '(line-number-mode 1)
 '(line-spacing 0.2)
 '(maximum-scroll-margin 0.01)
 '(mode-icons-mode nil)
 '(neo-window-fixed-size nil)
 '(nyan-mode nil)
 '(org-agenda-files
   (quote
    ("~/Code/notes/todo.org" "~/Apps/org-mode/todo.org")) t)
 '(package-selected-packages
   (quote
    (helm fit-frame flx-ido which-key comint-better-defaults terraform-mode beacon csv yaml-mode all-the-icons-dired web-mode iedit highlight-symbol bash-completion doom-modeline doom-themes anzu md4rd nv-delete-back flymd websocket flow-minor-mode auto-complete-exuberant-ctags ruby-tools smooth-scrolling find-file-in-project markdown-mode+ neotree exwm json-mode uuidgen csv-mode smartparens image+ rust-mode ace-window org-bullets git-gutter-fringe git-gutter-fringe+ linum-relative dockerfile-mode git-gutter magit enh-ruby-mode projectile better-defaults auto-dim-other-buffers rspec-mode rubocop company counsel ivy ruby-block ruby-additional robe relative-line-numbers multiple-cursors highlight-chars haml-mode git-commit diff-hl cl-lib-highlight bundler auto-complete)))
 '(persp-mode nil)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t)
 '(send-mail-function (quote mailclient-send-it))
 '(size-indication-mode nil)
 '(smooth-scrolling-mode t)
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(tramp-default-user "callum.neve-jones")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-attr-value-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#263238" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "nil" :family "Hack"))))
 '(auto-dim-other-buffers-face ((t (:background "gray10"))))
 '(beacon-fallback-background ((t (:background "#e8b81a"))))
 '(info-xref ((t (:inherit link))))
 '(mode-line-buffer-id ((t (:foreground "green2" :weight bold)))))

;; Bracket Highlighting
(show-paren-mode 1)

;; Themes
;;(load-theme 'material t)
;; For DOOM themes run "M-x all-the-icons-install-fonts" first
(require 'doom-themes)
(load-theme 'doom-nord t)
(doom-themes-neotree-config)
(doom-themes-org-config)

(require 'doom-modeline)
(doom-modeline-mode 1)

;; Midnight mode to clean buffers
(require 'midnight)

;; Toggle toolbar
(tool-bar-mode -1)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Font size
(set-face-attribute 'default nil :height 160)

;; delete-selection-mode
(delete-selection-mode 1)

;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-current-match t)

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
(setq projectile-enable-caching t)

;; Projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; enh-ruby-mode
(add-to-list 'auto-mode-alist
  '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(setq enh-ruby-add-encoding-comment-on-save nil)

;; Backups and Autosave file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; git-gutter-fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

;; Dockerfile-mode
;(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ruby-tools
(require 'ruby-tools)
(add-hook 'after-init-hook 'ruby-tools-mode)

;; Need to manualy require ruby-tools as it isn't required in enh-ruby-mode
(define-globalized-minor-mode my-global-ruby-tools-mode ruby-tools-mode
  (lambda () (ruby-tools-mode 1)))

(my-global-ruby-tools-mode 1)
;; C-‘ converts the thing into a single-quoted string
;; C-“ converts the thing into a double-quoted string
;; C-: converts the thing into a symbol
;; C-; clears a string

;; Kill buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Custom keys minor mode
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Duplicate line
    (define-key map (kbd "C-c C-v") "\C-a\C- \C-n\M-w\C-y")
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

;; Company
;; Does the autocomplete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0.2)

;; Windmove mode
(windmove-default-keybindings)

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Org mode agenda
(setq org-agenda-files (list "~/Apps/org-mode/todo.org"))

;; electric-pair-mode
(electric-pair-mode 1)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'set-rectangular-region-anchor)

(global-set-key (kbd "C-S-c C-S-c") 'mc/esyndit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ace-window
(require 'ace-window)
(global-set-key (kbd "M-[") 'ace-window)
(require 'shell)
(define-key shell-mode-map (kbd "M-[") 'ace-window)

;; Resize windows
(global-set-key (kbd "<M-up>") 'shrink-window)
(global-set-key (kbd "<M-down>") 'enlarge-window)
(global-set-key (kbd "<M-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-right>") 'enlarge-window-horizontally)

;; Smartparens
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

;; flx-ido (projectile fuzzy search)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq completion-auto-help nil)

;; Ido buffer intuitive navigation
(add-hook 'ido-setup-hook '(lambda ()
                             (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
                             (define-key ido-completion-map "\C-n" 'ido-next-match)
                             (define-key ido-completion-map "\C-f" 'ido-next-match)
                             (define-key ido-completion-map "\C-p" 'ido-prev-match)
                             (define-key ido-completion-map "\C-b" 'ido-prev-match)
                             (define-key ido-completion-map " " 'ido-exit-minibuffer)
                             ))

;; Truncate shell output
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; Auto revert buffers
(global-auto-revert-mode 1)

;; Save even without making changes to the file
(defun save-buffer-always ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(global-set-key (kbd "C-x C-s") 'save-buffer-always)

;; Set pager to cat
(setenv "PAGER" "cat")

;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
;; Every time when the neotree window is opened, let it find current file and jump to node
(setq neo-smart-open t)
;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically.
(setq projectile-switch-project-action 'neotree-projectile-action)

;;NeoTree can be opened (toggled) at projectile project root as follows:
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; find-file-in-project
;; Use fd
(setq ffip-use-rust-fd t)

;; expand-region (select within quotes, brackets etc)
(global-set-key (kbd "C-.") 'er/expand-region)

(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Disable mouse
(require 'disable-mouse)
;;(global-disable-mouse-mode)

;; Scroll buffer by quarters
(defun window-quarter-height ()
  (max 1 (/ (1- (window-height (selected-window))) 4)))

(defun scroll-up-quarter ()
  (interactive)
  (scroll-up (window-quarter-height)))

(defun scroll-down-quarter ()
  (interactive)
  (scroll-down (window-quarter-height)))

'(scroll-preserve-screen-position t)
(global-set-key (kbd "M-n") 'scroll-up-quarter)
(global-set-key (kbd "M-p") 'scroll-down-quarter)

;; Open init.el shortcut
(defun er-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'er-find-user-init-file)

;; Copy file path to killring
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "C-x p") 'my-put-file-name-on-clipboard)

;; Show search result count and increment
(global-anzu-mode +1)

;; insert "binding.pry"
(defun insert-binding-pry ()
  "Insert 'binding.pry' at cursor point"
  (interactive)
  (insert "binding.pry"))
(global-set-key (kbd "C-c b") 'insert-binding-pry)

(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)
(put 'downcase-region 'disabled nil)

;; web-mode for erb files
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; Remap backspace
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Whitespace highlighting
(setq-default show-trailing-whitespace t)
(add-hook 'shell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(customize-set-variable 'tramp-default-user "callum.neve-jones")

;; Beacon (cursor finder)
(beacon-mode 1)

(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Flyspell - map flyspell correct to mouse-3 rather than mouse-2
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(global-set-key (kbd "C-x r") 'rename-buffer)

(require 'bash-completion)
(bash-completion-setup)

(which-key-mode)

(add-hook 'shell-mode-hook
  (lambda ()
    (face-remap-set-base 'comint-highlight-prompt :inherit nil)))
