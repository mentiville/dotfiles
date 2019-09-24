(setq inhibit-startup-screen t)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
(set-default 'truncate-lines t)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode nil)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(defvar home-bin-dir (expand-file-name "~/.local/bin"))
(defvar cargo-bin-dir (expand-file-name "~/.cargo/bin"))

;; (and (file-exists-p home-bin-dir)
;;      (or
;;       (not (string-match-p (regexp-quote home-bin-dir) (getenv "PATH")))
;;       (not (member home-bin-dir exec-path)))
;;      )

(if (file-exists-p home-bin-dir)
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":" home-bin-dir))
      (setq exec-path (append exec-path (list home-bin-dir)))))

(if (file-exists-p cargo-bin-dir)
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":" cargo-bin-dir))
      (setq exec-path (append exec-path (list cargo-bin-dir)))))

(defvar nvm-bin (getenv "NVM_BIN"))

(if (and nvm-bin (file-exists-p nvm-bin))
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":" nvm-bin))
      (setq exec-path (append exec-path (list nvm-bin)))))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'package)
(let* ((proto "https")) 
 (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
 (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
 (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
 (package-initialize))

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

(custom-available-themes)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package magit
  :ensure t)

(use-package find-file-in-project
  :ensure t
  :config
  (global-set-key (kbd "C-x f") 'find-file-in-project))

(use-package yaml-mode
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrap- and expand-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wrap-region
  :ensure t
  :config
  (require 'wrap-region)
  (wrap-region-mode t)
  (wrap-region-add-wrappers
   '(("$" "$")
     ("{-" "-}" "#")
     ("/" "/" nil (ruby-mode javascript-mode))
     ("/* " " */" "#" (java-mode javascript-mode css-mode))
     ("`" "`" nil (markdown-mode ruby-mode)))))

(use-package expand-region
  :ensure t
  :config
  (require 'expand-region)
  (global-set-key (kbd "C-'") 'er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package company
  :ensure t
  :init (global-company-mode)
  :hook (prog-mode . company-mode)
  :config
  (progn    
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (define-globalized-minor-mode global-highlight-parentheses-mode
      highlight-parentheses-mode
      (lambda ()
	(highlight-parentheses-mode t)))
    (global-highlight-parentheses-mode t)

    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :ensure t
  :config
  (setq cider-test-infer-test-ns (lambda (ns) ns))
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package flycheck
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode
  :ensure t)

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package eglot
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-reveal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(package-selected-packages
   (quote
    (expand-region wrap-region ox-reveal htmlize yaml-mode find-file-in-project magit toml-mode cargo eglot flycheck flycheck-rust rust-mode tango-theme moe-theme monokai-theme monokai spacemacs-theme spacemacs-dark gruvbox-theme highlight-parentheses cider clojure-mode rainbow-delimiters company use-package paredit)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
