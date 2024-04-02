;;; init.el --- Solo Emacs (no external packages) Configuration
;;; Commentary:

;;; Code:


(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window))
  :custom
  (treesit-font-lock-level 4)
  (initial-scratch-message "")
  (ring-bell-function 'ignore)
  (truncate-lines t)
  (delete-selection-mode 1)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-lh")
  (inhibit-startup-message t)
  (make-backup-files nil)
  (ispell-dictionary "en_US")
  (create-lockfiles nil)
  :init
  (load-theme 'wombat)
  (set-face-attribute 'default nil :height 100)

  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (fido-vertical-mode)

  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
		 (window-height . 4)))
  
  (message (emacs-init-time)))


(use-package isearch
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?"))

(use-package vc
  ;; This is not needed, but it is left here as a reminder of some of the keybindings
  :bind
  (("C-x v d" . vc-dir)
   ("C-x v =" . vc-diff)
   ("C-x v D" . vc-root-diff)
   ("C-x v v" . vc-next-action)))

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-previous)))

(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-prefer-plaintext t)
  :init
  (setq eglot-stay-out-of '(flymake))
  :bind (:map
         eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-actions-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format)))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :custom
  (add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src"))
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

(use-package js-base-mode
  :defer 't
  :ensure js ;; I care about js-base-mode but it is locked behind the feature "js"
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (unbind-key "M-." js-base-mode-map))

(use-package typescript-ts-mode
  :ensure typescript-ts-mode
  :mode "\\.tsx?\\'"
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

(use-package rust-ts-mode
  :ensure rust-ts-mode
  :mode "\\.rs\\'"
  :defer 't
  :custom
  (rust-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

(use-package toml-ts-mode
  :ensure toml-ts-mode
  :mode "\\.toml\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))

(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/ikatyang/tree-sitter-markdown" "master" "src")))


(provide 'init)
;;; init.el ends here
