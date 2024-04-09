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
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (switch-to-buffer-obey-display-actions t)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (tab-width 4)
  (indent-tabs-mode nil)
  :config
  (set-face-attribute 'default nil :family "Hack" :height 100)

  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")
    (setq mac-command-modifier 'meta)
    (set-face-attribute 'default nil :family "Hack" :height 130))

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
                  (add-to-list 'result (cons num "deleted"))
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
    (remove-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))
  
  (defun emacs-solo/git-gutter-on ()
    (interactive)
    (emacs-solo/git-gutter-add-mark)
    (add-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (global-set-key (kbd "M-9") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "M-0") 'emacs-solo/goto-next-hunk)
  (global-set-key (kbd "C-c g p") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "C-c g r") 'emacs-solo/git-gutter-off)
  (global-set-key (kbd "C-c g g") 'emacs-solo/git-gutter-on)
  (global-set-key (kbd "C-c g n") 'emacs-solo/goto-next-hunk)
  

  ;; initialize customizations
  (emacs-solo/set-exec-path-from-shell-PATH)
  (add-hook 'emacs-lisp-mode-hook #'emacs-solo/elisp-mode-hook)

  ;; Disabled in favor of icomplete
  ;; (add-hook 'completion-list-mode-hook #'emacs-solo/jump-to-completions)
  
  :init
  (when scroll-bar-mode
    (scroll-bar-mode -1))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (winner-mode)
  (xterm-mouse-mode 1)
  (fido-vertical-mode)
  (file-name-shadow-mode 1)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  

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
      (slot . 1)))
   
   ))

;;; ERC
(use-package erc
  :defer t
  :custom
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
         (window-width . 0.2)
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
(use-package js-base-mode
  :defer 't
  :ensure js ;; I care about js-base-mode but it is locked behind the feature "js"
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (unbind-key "M-." js-base-mode-map))

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
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/ikatyang/tree-sitter-markdown" "master" "src")))

;;; EMACS-SOLO-THEME
(defun apply-emacs-solo-theme ()
  "Theme heavily inspired by Kanagawa Theme.
Available: https://github.com/meritamen/emacs-kanagawa-theme"
  (interactive)
  (defgroup emacs-solo-theme nil
    "Emacs-Solo-theme options."
    :group 'faces)

  (defcustom emacs-solo-theme-comment-italic t
    "Enable italics for comments and also disable background."
    :type 'boolean
    :group 'emacs-solo-theme)

  (defcustom emacs-solo-theme-keyword-italic t
    "Enable italics for keywords."
    :type 'boolean
    :group 'emacs-solo-theme)

  (defcustom emacs-solo-theme-org-height t
    "Use varying text heights for org headings."
    :type 'boolean
    :group 'emacs-solo-theme)

  (defcustom emacs-solo-theme-org-bold t
    "Inherit text bold for org headings"
    :type 'boolean
    :group 'emacs-solo-theme)

  (defcustom emacs-solo-theme-org-priority-bold t
    "Inherit text bold for priority items in agenda view"
    :type 'boolean
    :group 'emacs-solo-theme)

  (defcustom emacs-solo-theme-org-highlight nil
    "Highlight org headings."
    :type 'boolean
    :group 'emacs-solo-theme)

  (defcustom emacs-solo-theme-underline-parens t
    "If non-nil, underline matching parens when using `show-paren-mode' or similar."
    :type 'boolean
    :group 'emacs-solo-theme)

  (defcustom emacs-solo-theme-custom-colors nil
    "Specify a list of custom colors."
    :type 'alist
    :group 'emacs-solo-theme)

  (defun true-color-p ()
    (or (display-graphic-p)
    (= (tty-display-color-cells) 16777216)))

  (deftheme emacs-solo "An elegant theme inspired by The Great Wave off Emacs-Solo by Katsushika Hokusa")

  (eval
    (defvar emacs-solo-dark-palette
      `(
    ;; (fuji-white       ,(if (true-color-p) "#DCD7BA" "#ffffff"))
    (fuji-white       ,(if (true-color-p) "#ffffff" "#ffffff"))
    (old-white        ,(if (true-color-p) "#C8C093" "#ffffff"))
    (sumi-ink-0       ,(if (true-color-p) "#16161D" "#000000"))
    ;; (sumi-ink-1b      ,(if (true-color-p) "#1f1f28" "#000000"))
    (sumi-ink-1b      ,(if (true-color-p) "#1e1e2e" "#000000"))
    (sumi-ink-1       ,(if (true-color-p) "#1F1F28" "#080808"))
    (sumi-ink-2       ,(if (true-color-p) "#2A2A37" "#121212"))
    (sumi-ink-3       ,(if (true-color-p) "#363646" "#303030"))
    (sumi-ink-4       ,(if (true-color-p) "#54546D" "#303030"))
    (wave-blue-1      ,(if (true-color-p) "#223249" "#4e4e4e"))
    (wave-blue-2      ,(if (true-color-p) "#2D4F67" "#585858"))
    (wave-aqua-1      ,(if (true-color-p) "#6A9589" "#6a9589"))
    (wave-aqua-2      ,(if (true-color-p) "#7AA89F" "#717C7C"))
    (winter-green     ,(if (true-color-p) "#2B3328" "#585858"))
    (winter-yellow    ,(if (true-color-p) "#49443C" "#585858"))
    (winter-red       ,(if (true-color-p) "#43242B" "#585858"))
    (winter-blue      ,(if (true-color-p) "#252535" "#585858"))
    (autumn-green     ,(if (true-color-p) "#76946A" "#585858"))
    (autumn-red       ,(if (true-color-p) "#C34043" "#585858"))
    (autumn-yellow    ,(if (true-color-p) "#DCA561" "#585858"))
    (samurai-red      ,(if (true-color-p) "#E82424" "#585858"))
    (ronin-yellow     ,(if (true-color-p) "#FF9E3B" "#585858"))
    (dragon-blue      ,(if (true-color-p) "#658594" "#658594"))
    ;; (fuji-gray        ,(if (true-color-p) "#727169" "#717C7C"))
    (fuji-gray        ,(if (true-color-p) "#6c7086" "#717C7C"))
    (spring-violet-1  ,(if (true-color-p) "#938AA9" "#717C7C"))
    (oni-violet       ,(if (true-color-p) "#957FB8" "#717C7C"))
    (crystal-blue     ,(if (true-color-p) "#7E9CD8" "#717C7C"))
    (spring-violet-2  ,(if (true-color-p) "#9CABCA" "#717C7C"))
    (spring-blue      ,(if (true-color-p) "#7FB4CA" "#717C7C"))
    (light-blue       ,(if (true-color-p) "#A3D4D5" "#717C7C"))
    ;; (spring-green     ,(if (true-color-p) "#98BB6C" "#717C7C"))
    (spring-green     ,(if (true-color-p) "#a0da9c" "#717C7C"))
    (boat-yellow-1    ,(if (true-color-p) "#938056" "#717C7C"))
    ;; (boat-yellow-2    ,(if (true-color-p) "#C0A36E" "#717C7C"))
    (boat-yellow-2    ,(if (true-color-p) "#ec03ed" "#717C7C"))
    (light-yellow     ,(if (true-color-p) "#f9e2af" "#717C7C"))
    (carp-yellow      ,(if (true-color-p) "#E6C384" "#717C7C"))
    (sakura-pink      ,(if (true-color-p) "#D27E99" "#717C7C"))
    (wave-red         ,(if (true-color-p) "#E46876" "#717C7C"))
    (peach-red        ,(if (true-color-p) "#FF5D62" "#717C7C"))
    (surimi-orange    ,(if (true-color-p) "#FFA066" "#717C7C"))
    (katana-gray      ,(if (true-color-p) "#717C7C" "#717C7C"))
    (comet            ,(if (true-color-p) "#54536D" "#4e4e4e")))))

  (defmacro define-emacs-solo-dark-theme (theme &rest faces)
    `(let ((class '((class color) (min-colors 89)))
           ,@emacs-solo-dark-palette)
       (cl-loop for (cvar . val) in emacs-solo-theme-custom-colors
        do (set cvar val))
       (custom-theme-set-faces ,theme ,@faces)))

  (define-emacs-solo-dark-theme
   'emacs-solo
   ;; Customize faces
   `(default                                       ((,class (:background ,sumi-ink-1b :foreground ,fuji-white))))
   `(border                                        ((,class (:background ,sumi-ink-1b :foreground ,sumi-ink-0))))
   `(button                                        ((,class (:foreground ,wave-aqua-2))))
   `(child-frame                                   ((,class (:background ,sumi-ink-0 :foreground ,sumi-ink-0))))
   `(child-frame-border                            ((,class (:background ,sumi-ink-0 :foreground ,sumi-ink-0))))
   `(cursor                                        ((,class (:background ,light-blue :foreground ,sumi-ink-0 :weight bold))))
   `(error                                         ((,class (:foreground ,samurai-red))))
   `(fringe                                        ((,class (:foreground ,sumi-ink-3))))
   `(glyph-face                                    ((,class (:background ,sumi-ink-4))))
   `(glyphless-char                                ((,class (:foreground ,sumi-ink-4))))
   `(header-line                                   ((,class (:background ,sumi-ink-0))))
   `(highlight                                     ((,class (:background ,comet :foreground ,spring-violet-1))))
   `(hl-line                                       ((,class (:background ,sumi-ink-2))))
   `(homoglyph                                     ((,class (:foreground ,light-blue))))
   `(internal-border                               ((,class (:background ,sumi-ink-1b))))
   `(line-number                                   ((,class (:foreground ,sumi-ink-4))))
   `(line-number-current-line                      ((,class (:foreground ,spring-violet-2 :background ,sumi-ink-2 :weight bold))))
   `(lv-separator                                  ((,class (:foreground ,wave-blue-2 :background ,sumi-ink-2))))
   `(match                                         ((,class (:background ,carp-yellow :foreground ,sumi-ink-0))))
   `(menu                                          ((,class (:background ,sumi-ink-0 :foreground ,fuji-white))))
   `(mode-line                                     ((,class (:background ,sumi-ink-0))))
   `(mode-line-inactive                            ((,class (:background unspecified :foreground ,fuji-white))))
   `(mode-line-active                              ((,class (:background ,sumi-ink-0 :foreground ,fuji-white))))
   `(mode-line-highlight                           ((,class (:foreground ,boat-yellow-2))))
   `(mode-line-buffer-id                           ((,class (:foreground ,wave-aqua-2 :weight bold))))
   `(numbers                                       ((,class (:background ,sakura-pink))))
   `(region                                        ((,class (:background ,wave-blue-2))))
   `(separator-line                                ((,class (:background ,sumi-ink-0))))
   `(shadow                                        ((,class (:background ,sumi-ink-0))))
   `(success                                       ((,class (:foreground ,wave-aqua-2))))
   `(vertical-border                               ((,class (:foreground ,sumi-ink-4))))
   `(warning                                       ((,class (:foreground ,ronin-yellow))))
   `(window-border                                 ((,class (:background ,sumi-ink-1b))))
   `(window-divider                                ((,class (:foreground ,sumi-ink-2))))
   `(hi-yellow                                     ((,class (:background ,carp-yellow :foreground ,sumi-ink-1b))))

   ;; Tabs
   `(tab-bar                                       ((,class (:background ,sumi-ink-1b))))
   `(tab-bar-tab-inactive                          ((,class (:foreground ,sumi-ink-4 :background ,sumi-ink-1b))))
   
   ;; Font lock
   `(font-lock-type-face                           ((,class (:foreground ,light-yellow))))
   `(font-lock-regexp-grouping-backslash           ((,class (:foreground ,boat-yellow-2))))
   `(font-lock-keyword-face                        ((,class (:foreground ,oni-violet :weight semi-bold :slant ,(if emacs-solo-theme-keyword-italic 'italic 'normal)))))
   `(font-lock-warning-face                        ((,class (:foreground ,ronin-yellow))))
   `(font-lock-string-face                         ((,class (:foreground ,spring-green :slant italic))))
   `(font-lock-builtin-face                        ((,class (:foreground ,spring-blue))))
   `(font-lock-reference-face                      ((,class (:foreground ,peach-red))))
   `(font-lock-constant-face                       ((,class (:foreground ,carp-yellow))))
   `(font-lock-function-name-face                  ((,class (:foreground ,crystal-blue))))
   `(font-lock-variable-name-face                  ((,class (:foreground ,wave-red))))
   `(font-lock-negation-char-face                  ((,class (:foreground ,peach-red))))
   `(font-lock-comment-face                        ((,class (:foreground ,fuji-gray :slant ,(if emacs-solo-theme-keyword-italic 'italic 'normal)))))
   `(font-lock-comment-delimiter-face              ((,class (:foreground ,fuji-gray :slant ,(if emacs-solo-theme-keyword-italic 'italic 'normal)))))
   `(font-lock-doc-face                            ((,class (:foreground ,comet))))
   `(font-lock-doc-markup-face                     ((,class (:foreground ,comet))))
   `(font-lock-preprocessor-face                   ((,class (:foreground ,light-yellow))))
   `(elisp-shorthand-font-lock-face                ((,class (:foreground ,fuji-white))))
   `(info-xref                                     ((,class (:foreground ,carp-yellow))))
   `(minibuffer-prompt-end                         ((,class (:foreground ,autumn-red :background ,winter-red))))
   `(minibuffer-prompt                             ((,class (:foreground ,carp-yellow))))
   `(epa-mark                                      ((,class (:foreground ,wave-red))))
   `(dired-mark                                    ((,class (:foreground ,wave-red))))
   `(trailing-whitespace                           ((,class (:background ,comet))))
   `(mode-line                                     ((,class (:background ,sumi-ink-0 :foreground ,fuji-white :weight bold))))

   ;; message colors
   `(message-header-name                           ((,class (:foreground ,sumi-ink-4))))
   `(message-header-other                          ((,class (:foreground ,surimi-orange))))
   `(message-header-subject                        ((,class (:foreground ,carp-yellow))))
   `(message-header-to                             ((,class (:foreground ,old-white))))
   `(message-header-cc                             ((,class (:foreground ,wave-aqua-2))))
   `(message-header-xheader                        ((,class (:foreground ,old-white))))
   `(custom-link                                   ((,class (:foreground ,crystal-blue))))
   `(link                                          ((,class (:foreground ,crystal-blue))))

   ;; org-mode
   `(org-done                                      ((,class (:foreground ,dragon-blue))))
   `(org-code                                      ((,class (:background ,sumi-ink-0))))
   `(org-meta-line                                 ((,class (:background ,winter-green :foreground ,spring-green))))
   `(org-block                                     ((,class (:background ,sumi-ink-0 :foreground ,sumi-ink-4))))
   `(org-block-begin-line                          ((,class (:background ,winter-blue :foreground ,spring-blue))))
   `(org-block-end-line                            ((,class (:background ,winter-red :foreground ,peach-red))))
   `(org-headline-done                             ((,class (:foreground ,dragon-blue :strike-through t))))
   `(org-todo                                      ((,class (:foreground ,surimi-orange :weight bold))))
   `(org-headline-todo                             ((,class (:foreground ,sumi-ink-2))))
   `(org-upcoming-deadline                         ((,class (:foreground ,peach-red))))
   `(org-footnote                                  ((,class (:foreground ,wave-aqua-2))))
   `(org-indent                                    ((,class (:background ,sumi-ink-1b :foreground ,sumi-ink-1b))))
   `(org-hide                                      ((,class (:background ,sumi-ink-1b :foreground ,sumi-ink-1b))))
   `(org-date                                      ((,class (:foreground ,wave-blue-2))))
   `(org-ellipsis                                  ((,class (:foreground ,wave-blue-2 :weight bold))))
   `(org-level-1                                   ((,class (:inherit bold :foreground ,peach-red :height ,(if emacs-solo-theme-org-height 1.3 1.0) :weight ,(if emacs-solo-theme-org-bold 'unspecified 'normal)))))
   `(org-level-2                                   ((,class (:inherit bold :foreground ,spring-violet-2 :height ,(if emacs-solo-theme-org-height 1.2 1.0) :weight ,(if emacs-solo-theme-org-bold 'unspecified 'normal)))))
   `(org-level-3                                   ((,class (:foreground ,boat-yellow-2 :height ,(if emacs-solo-theme-org-height 1.1 1.0)))))
   `(org-level-4                                   ((,class (:foreground ,fuji-white))))
   `(org-level-5                                   ((,class (:foreground ,fuji-white))))
   `(org-level-6                                   ((,class (:foreground ,carp-yellow))))
   `(org-level-7                                   ((,class (:foreground ,surimi-orange))))
   `(org-level-8                                   ((,class (:foreground ,spring-green))))
   `(org-priority                                  ((,class (:foreground ,peach-red :inherit bold :weight ,(if emacs-solo-theme-org-priority-bold 'unspecified 'normal)))))

   `(info-header-xref                              ((,class (:foreground ,carp-yellow))))
   `(xref-file-header                              ((,class (:foreground ,carp-yellow))))
   `(xref-match                                    ((,class (:foreground ,carp-yellow))))

   ;; show-paren
   `(show-paren-match                              ((,class (:background ,wave-aqua-1 :foreground ,sumi-ink-0 :weight bold :underline ,(when emacs-solo-theme-underline-parens t)))))
   `(show-paren-match-expression                   ((,class (:background ,wave-aqua-1 :foreground ,sumi-ink-0 :weight bold))))
   `(show-paren-mismatch                           ((,class (:background ,peach-red :foreground ,old-white :underline ,(when emacs-solo-theme-underline-parens t)))))
   `(tooltip                                       ((,class (:foreground ,sumi-ink-0 :background ,carp-yellow :weight bold))))

   ;; term
   `(term                                          ((,class (:background ,sumi-ink-0 :foreground ,fuji-white))))
   `(term-color-blue                               ((,class (:background ,crystal-blue :foreground ,crystal-blue))))
   `(term-color-bright-blue                        ((,class (:inherit term-color-blue))))
   `(term-color-green                              ((,class (:background ,wave-aqua-2 :foreground ,wave-aqua-2))))
   `(term-color-bright-green                       ((,class (:inherit term-color-green))))
   `(term-color-black                              ((,class (:background ,sumi-ink-0 :foreground ,fuji-white))))
   `(term-color-bright-black                       ((,class (:background ,sumi-ink-1b :foreground ,sumi-ink-1b))))
   `(term-color-white                              ((,class (:background ,fuji-white :foreground ,fuji-white))))
   `(term-color-bright-white                       ((,class (:background ,old-white :foreground ,old-white))))
   `(term-color-red                                ((,class (:background ,peach-red :foreground ,peach-red))))
   `(term-color-bright-red                         ((,class (:background ,spring-green :foreground ,spring-green))))
   `(term-color-yellow                             ((,class (:background ,carp-yellow :foreground ,carp-yellow))))
   `(term-color-bright-yellow                      ((,class (:background ,carp-yellow :foreground ,carp-yellow))))
   `(term-color-cyan                               ((,class (:background ,spring-blue :foreground ,spring-blue))))
   `(term-color-bright-cyan                        ((,class (:background ,spring-blue :foreground ,spring-blue))))
   `(term-color-magenta                            ((,class (:background ,spring-violet-2 :foreground ,spring-violet-2))))
      `(term-color-bright-magenta                     ((,class (:background ,spring-violet-2 :foreground ,spring-violet-2))))
   `(ansi-color-green                              ((,class (:foreground ,spring-green))))
   `(ansi-color-black                              ((,class (:background ,sumi-ink-0))))
   `(ansi-color-cyan                               ((,class (:foreground ,wave-aqua-2))))
     `(ansi-color-magenta                            ((,class (:foreground ,sakura-pink))))
   `(ansi-color-blue                               ((,class (:foreground ,crystal-blue))))
   `(ansi-color-red                                ((,class (:foreground ,peach-red))))
   `(ansi-color-white                              ((,class (:foreground ,fuji-white))))
   `(ansi-color-yellow                             ((,class (:foreground ,autumn-yellow))))
   `(ansi-color-bright-white                       ((,class (:foreground ,old-white))))
   `(ansi-color-bright-white                       ((,class (:foreground ,old-white))))

   ;; eglot
   `(eglot-inlay-hint-face      ((,class (:foreground ,fuji-gray :background ,sumi-ink-1b :height 0.8))))

   `(focus-unfocused                               ((,class (:foreground ,sumi-ink-4)))))

  (provide-theme 'emacs-solo)
  
  (defun emacs-solo-theme ()
    "Set emacs-solo theme"
    (interactive)
    (enable-theme 'emacs-solo))
  (emacs-solo-theme))

(apply-emacs-solo-theme)

(provide 'init)
;;; init.el ends here
