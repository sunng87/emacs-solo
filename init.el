;;; init.el --- Solo Emacs (no external packages) Configuration
;;; Commentary:

;;; Code:

(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

;;; EMACS
(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window)
   ("C-x C-b" . ibuffer))
  :custom
  (column-number-mode t)
  (completions-format 'one-column)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-al --group-directories-first")
  (global-auto-revert-non-file-buffers t)
  (history-length 25)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (shr-use-colors nil)
  (switch-to-buffer-obey-display-actions t)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (tab-width 4)
  (use-dialog-box nil)
  :config

  ;; Configure fonts per OS
  (set-face-attribute 'default nil :family "Hack" :height 100)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")
    (setq mac-command-modifier 'meta)
    (set-face-attribute 'default nil :family "Hack" :height 130))

  ;; Configure the Modus Themes' appearance
  (setq modus-themes-mode-line '(accented borderless)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intentse)
        modus-themes-prompts '(bold intense)
        ;; modus-themes-syntax '(faint alt-syntax green-strings yellow-comments) ;; all options
        modus-themes-syntax '(alt-syntax green-strings yellow-comments)
        modus-themes-org-blocks 'tinted-background
        modus-themes-scale-headings t
        modus-themes-region '(bg-only)
        modus-themes-headings
        '((1 . (rainbown overline background 1.4))
          (2 . (rainbown background 1.3))
          (3 . (rainbown bold 1.2))
          (4 . (semilight 1.1))
          ))
  (load-theme 'modus-vivendi t)

  (global-set-key (kbd "C-c p") (lambda ()
                  (interactive)
                  (shell-command (concat "prettier --write " (shell-quote-argument (buffer-file-name))))
                  (revert-buffer t t t)))

  (global-set-key (kbd "C-v") (lambda ()
                (interactive)
                (scroll-up-command)
                (recenter)
                ))
  (global-set-key (kbd "M-v") (lambda ()
                (interactive)
                (scroll-down-command)
                (unless (= (window-start) (point-min))
                  (recenter))
                (when (= (window-start) (point-min))
                  (let ((midpoint (/ (window-height) 2)))
                    (goto-char (window-start))
                    (forward-line midpoint)
                    (recenter midpoint)))))

  (defun emacs-solo/prefer-tabs ()
    "Disables indent-tabs-mode, and prefer spaces over tabs."
    (interactive)
    (indent-tabs-mode -1))

  (defun emacs-solo/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment the same as user Shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
                "[ \t\n]*$" "" (shell-command-to-string
                        "$SHELL --login -c 'echo $PATH'"
                        ))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

  (defun emacs-solo/elisp-mode-hook ()
    (interactive)
    (outline-minor-mode 1)
    (outline-hide-sublevels 1))

  (defun emacs-solo/jump-to-completions ()
    "Hook function to move focus to *Completions* buffer."
    (when (string= (buffer-name) "*Completions*")
      (goto-char (point-min))
      (switch-to-buffer-other-window "*Completions*")))

  (defun emacs-solo/goto-next-hunk ()
    "Jump cursor to the closest next hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
            (sorted-line-numbers (sort line-numbers '<))
           (next-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the next closest line number
                (cl-find-if (lambda (line) (> line current-line)) sorted-line-numbers)
              ;; If the current line is in the list, find the next line number that is not consecutive
              (let ((last-line nil))
                (cl-loop for line in sorted-line-numbers
                         when (and (> line current-line)
                                   (or (not last-line)
                                       (/= line (1+ last-line))))
                         return line
                         do (setq last-line line))))))

        (when next-line-number
        (goto-line next-line-number))))

  (defun emacs-solo/goto-previous-hunk ()
    "Jump cursor to the closest previous hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
             (line-numbers (mapcar #'car git-gutter-diff-info))
             (sorted-line-numbers (sort line-numbers '<))
           (previous-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the previous closest line number
                (cl-find-if (lambda (line) (< line current-line)) (reverse sorted-line-numbers))
              ;; If the current line is in the list, find the previous line number that has no direct predecessor
              (let ((previous-line nil))
                (dolist (line sorted-line-numbers)
                  (when (and (< line current-line)
                             (not (member (1- line) line-numbers)))
                    (setq previous-line line)))
                previous-line))))

        (when previous-line-number
        (goto-line previous-line-number))))
  

  (defun emacs-solo/git-gutter-process-git-diff ()
    "Process git diff for adds/mods/removals. Still doesn't diff adds/mods."
    (interactive)
    (let* ((file-path (buffer-file-name))
           (grep-command (if (eq system-type 'darwin)
                             "ggrep -Po"
                           "grep -Po"))
           (output (shell-command-to-string (format "git diff --unified=0 %s | %s '^@@ -[0-9]+(,[0-9]+)? \\+\\K[0-9]+(,[0-9]+)?(?= @@)'" file-path grep-command))))
      (setq lines (split-string output "\n"))
      (setq result '())
      (dolist (line lines)
        (if (string-match "\\(^[0-9]+\\),\\([0-9]+\\)\\(?:,0\\)?$" line)
            (let ((num (string-to-number (match-string 1 line)))
                  (count (string-to-number (match-string 2 line))))
              (if (= count 0)
                  (add-to-list 'result (cons (+ 1 num) "deleted"))
                (dotimes (i count)
                  (add-to-list 'result (cons (+ num i) "added"))))))
        (if (string-match "\\(^[0-9]+\\)$" line)
            (add-to-list 'result (cons (string-to-number line) "added")))))
    (setq-local git-gutter-diff-info result)
    result)

  (defun emacs-solo/git-gutter-add-mark ()
    "Add symbols to the margin based on Git diff statuses."
    (interactive)
    (let ((lines-status (emacs-solo/git-gutter-process-git-diff)))
      (remove-overlays)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-num (line-number-at-pos))
                 (status (cdr (assoc line-num lines-status))))
            (when status
              (move-to-column 0 t)
              (let ((overlay (make-overlay (point-at-bol) (point-at-eol))))
                (overlay-put overlay 'before-string
                             (propertize (if (string= status "added") "+" "-")
                                         'face (if (string= status "added")
                                                   '(:foreground "lightgreen" :background "lightgreen")
                                                 '(:foreground "tomato" :background "tomato")))))
              (forward-line))
            (unless status
              (move-to-column 0 t)
              (let ((overlay (make-overlay (point-at-bol) (point-at-bol))))
                (overlay-put overlay 'before-string " "))
              (forward-line)))))))

  (defun emacs-solo/git-gutter-off ()
    "Greedly remove all git gutter marks and other overlays."
    (interactive)
    (remove-overlays)
    (remove-hook 'pre-command-hook #'emacs-solo/git-gutter-add-mark)
    (remove-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (defun emacs-solo/git-gutter-on ()
    (interactive)
    (emacs-solo/git-gutter-add-mark)
    (add-hook 'pre-command-hook #'emacs-solo/git-gutter-add-mark)
    (add-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (global-set-key (kbd "M-9") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "M-0") 'emacs-solo/goto-next-hunk)
  (global-set-key (kbd "C-c g p") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "C-c g r") 'emacs-solo/git-gutter-off)
  (global-set-key (kbd "C-c g g") 'emacs-solo/git-gutter-on)
  (global-set-key (kbd "C-c g n") 'emacs-solo/goto-next-hunk)

  (emacs-solo/set-exec-path-from-shell-PATH)
  (add-hook 'emacs-lisp-mode-hook #'emacs-solo/elisp-mode-hook)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'emacs-solo/prefer-tabs)

  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Disabled in favor of icomplete
  ;; (add-hook 'completion-list-mode-hook #'emacs-solo/jump-to-completions)

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1)
  (fido-vertical-mode)
  (file-name-shadow-mode 1)

  (with-current-buffer (get-buffer-create "*scratch*")
    (insert (format ";;
;; ███████╗███╗   ███╗ █████╗  ██████╗███████╗    ███████╗ ██████╗ ██╗      ██████╗
;; ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝    ██╔════╝██╔═══██╗██║     ██╔═══██╗
;; █████╗  ██╔████╔██║███████║██║     ███████╗    ███████╗██║   ██║██║     ██║   ██║
;; ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║    ╚════██║██║   ██║██║     ██║   ██║
;; ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║    ███████║╚██████╔╝███████╗╚██████╔╝
;; ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝    ╚══════╝ ╚═════╝ ╚══════╝ ╚═════╝
;;
;;   Loading time : %s
;;   Packages     : %s
;;
"
  (emacs-init-time)
  (number-to-string (length package-activated-list)))))

  (message (emacs-init-time)))

;;; WINDOW
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*.*e?shell\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1)))))

;;; ERC
(use-package erc
  :defer t
  :custom
  (erc-join-buffer 'window)
  ;; (erc-interactive-display ...) ;; this option will be available on next ERC release (5.6)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-timestamp-format "[%H:%M]")
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs"))))

;;; ICOMPLETE
(use-package icomplete
  :bind
  ((:map icomplete-minibuffer-map
     ("C-n" . icomplete-forward-completions)
     ("C-p" . icomplete-backward-completions)
     ("C-n" . icomplete-forward-completions)
     ("C-v" . icomplete-vertical-toggle)
     ("RET" . icomplete-force-complete-and-exit)
     ))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq completion-auto-help nil)
  (fido-mode -1)
  (icomplete-mode 1))

;;; DIRED
(use-package dired
  :bind
  (("M-i" . emacs-solo/window-dired-vc-root-left))
  :init
  (defun emacs-solo/window-dired-vc-root-left (&optional directory-path)
    "Creates *Dired-Side* like an IDE side explorer"
    (interactive)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)

    (let ((dir (if directory-path
                   (dired-noselect directory-path)
         (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir))))))

      (display-buffer-in-side-window
       dir `((side . left)
         (slot . 0)
         (window-width . 30)
         (window-parameters . ((no-other-window . t)
                   (no-delete-other-windows . t)
                   (mode-line-format . (" "
                            "%b"))))))
      (with-current-buffer dir
    (let ((window (get-buffer-window dir)))
          (when window
            (select-window window)
        (rename-buffer "*Dired-Side*")
        )))))

  (defun emacs-solo/window-dired-open-directory ()
    "Open the current directory in *Dired-Side* side window."
    (interactive)
    (emacs-solo/window-dired-vc-root-left (dired-get-file-for-visit)))

  (eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "G") 'emacs-solo/window-dired-open-directory))))
  

;;; ESHELL
(use-package eshell
  :after (:all emacs)
  :config
  (require 'vc)
  (add-hook 'eshell-mode-hook
        (lambda ()
              (local-set-key (kbd "C-l")
                 (lambda ()
                               (interactive)
                               (eshell/clear 1)
                   (eshell-send-input)
                   ))))

  (setq eshell-prompt-function
    (lambda ()
          (concat
           "┌─("
       (if (> eshell-last-command-status 0)
           "⛒"
         "✓")
       " "
       (number-to-string eshell-last-command-status)
           ")──("
       "Ꜫ"
       " "
       (user-login-name)
           ")──("
       "⏲"
       " "
           (format-time-string "%H:%M:%S" (current-time))
           ")──("
       "🗁"
       " "
           (concat (if (>= (length (eshell/pwd)) 40)
               (concat "..." (car (last (butlast (split-string (eshell/pwd) "/") 0))))
             (abbreviate-file-name (eshell/pwd))))
           ")\n"
       (if (car (vc-git-branches))
           (concat
        "├─("
        "⎇"
        " "
        (car (vc-git-branches))
        ")\n"
        ))
           "└─➜ ")))

  (setq eshell-prompt-regexp "└─➜ ")

  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

  (setq eshell-visual-commands
        '("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
          "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m"
          "ncmpcpp" "newsbeuter" "nethack" "mutt")))

;;; ISEARCH
(use-package isearch
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?"))

;;; VC
(use-package vc
  ;; This is not needed, but it is left here as a reminder of some of the keybindings
  :bind
  (("C-x v d" . vc-dir)
   ("C-x v =" . vc-diff)
   ("C-x v D" . vc-root-diff)
   ("C-x v v" . vc-next-action)))

;;; SMERGE
(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-previous)))

;;; ELDOC
(use-package eldoc
  :init
  (global-eldoc-mode))

;;; EGLOT
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-prefer-plaintext t)
  :init
  (defun emacs-solo/eglot-setup ()
    "Setup eglot mode with specific exclusions."
    (unless (eq major-mode 'emacs-lisp-mode)
      (eglot-ensure)))

  (add-hook 'prog-mode-hook #'emacs-solo/eglot-setup)

  :bind (:map
         eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-actions-organize-imports)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

;;; FLYMAKE
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

;;; RUBY-TS-MODE
(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :custom
  (add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src"))
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

;;; JS-TS-MODE
(use-package js-ts-mode
  :ensure js ;; I care about js-base-mode but it is locked behind the feature "js"
  :mode "\\.jsx?\\'"    
  :defer 't
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")))

;;; TYPESCRIPT-TS-MODE
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

;;; RUST-TS-MODE
(use-package rust-ts-mode
  :ensure rust-ts-mode
  :mode "\\.rs\\'"
  :defer 't
  :custom
  (rust-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")))

;;; TOML-TS-MODE
(use-package toml-ts-mode
  :ensure toml-ts-mode
  :mode "\\.toml\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))

;;; MARKDOWN-TS-MODE
(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  :config
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "main" "src")))

(provide 'init)
;;; init.el ends here
