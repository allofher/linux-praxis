;;
;; built in basic config stuff

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1) 
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(setq-default cursor-type 'bar)
(global-auto-revert-mode 1)

;;
;;package management setup

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
(setenv "PATH" (concat (getenv "PATH") ":/home/oliver/.local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/home/oliver/go/bin"))
(setq exec-path (append exec-path '("/home/oliver/.local/bin")))
(setq exec-path (append exec-path '("/home/oliver/go/bin")))

;;
;;packages

;icons for rendering compatability across packages
(use-package all-the-icons)

;git
(use-package magit)

;tree-sitter
(use-package tree-sitter)
(use-package tree-sitter-langs)
(global-tree-sitter-mode)

;autocomplete info and text search in interaction bar
(use-package ivy
  :bind
  (:map ivy-minibuffer-map
	("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

;extra info on keys associated with functions
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;additional functionality versions of base functions
(use-package counsel
  :bind (("M-x" . 'counsel-M-x)
	 :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))
;removing the "^" from ivy's default string with counsel-M-x
(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

;added description rendering for functions and variables
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;hotkey management
(use-package general
  :config
  (general-create-definer my-leader
   :keymaps 'override
   :prefix "C-SPC")) 

;gives "project" functionality to folders
(use-package projectile
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/")
    (setq projectile-project-search-path '("~/projects/")))
  (setq projectile-switch-project-action #'projectile-dired))

;make projectile easier to parse
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;pdftools :)
(use-package pdf-tools
  :config (pdf-tools-install))
  
;custom functions that let you repeat inputs
(use-package hydra)

;lets you flip through buffers and kill them as you please
(defhydra hydra-buffers (:timeout 10)
  "manage buffers"
  ("n" switch-to-next-buffer "next")
  ("e" switch-to-prev-buffer "prev")
  ("t" kill-this-buffer "terminate")
  ("f" nil "finished" :exit t))

;allows scaling of text in and out
(defhydra hydra-scale-text (:timeout 10)
  "scale text"
  ("n" text-scale-increase "in")
  ("e" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;dev setup
;lsp mode setup and packages -- for language server while doing development
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)) 

(defun lsp-mode-setup ()
  (setq lsp-header-line-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-mode-setup)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package golsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;using lsp mode when python loads
(use-package python-mode
  :hook (python-mode . lsp-deferred))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package go-mode)
(add-hook 'go-mode-hook 'lsp-deferred)

;beginning of preparation packages and functions for org-mode
;general functionality
(defface my-org-custom-highlight-face
  '((t :background "#B54A65")) ; Replace #B54A65 with your desired hexadecimal color
  "Custom face for highlighting keyphrases in Org mode."
  :group 'my-org-faces)

(defun org-custom-html-highlight ()
  (interactive)
  (hi-lock-set-pattern "\\(\\?note\\)\\|\\(\\?end\\)\\|\\(\\?highb\\)\\|\\(\\?high\\)" 'my-org-custom-highlight-face))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(require 'org-faces)

(defun org-mode-setup ()
  (org-indent-mode)
  (hi-lock-mode 1)
  (org-custom-html-highlight)
  (variable-pitch-mode 1)
  (display-line-numbers-mode 0)
  (visual-line-mode 1)
  (setq header-line-format " "))

;font and feel management
(set-face-attribute 'default nil :font "Liberation Mono")
(set-face-attribute 'fixed-pitch nil :font "Liberation Mono")
(set-face-attribute 'variable-pitch nil :font "Liberation Serif")

(defun org-font-setup ()
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Liberation Sans" :weight 'bold :height (cdr face)))
  (set-face-attribute 'header-line nil :background "#1d1f21" :height 2.5 :inherit 'variable-pitch)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch :background "#303632")
  (set-face-attribute 'org-block-begin-line nil :foreground "#ffb3b3" :inherit 'fixed-pitch :background "#303632")
  (set-face-attribute 'org-block-end-line nil :foreground "#ffb3b3" :inherit 'fixed-pitch :background "#303632")
  (set-face-attribute 'org-document-info-keyword nil :foreground "#ffb3b3" :inherit 'fixed-pitch :background "#303632")
  (set-face-attribute 'org-document-info nil :foreground "#ffb3b3" :inherit 'fixed-pitch :background "#303632")
  (set-face-attribute 'org-document-title nil :foreground "#ffb3b3" :inherit 'fixed-pitch :background "#303632")
  (set-face-attribute 'org-list-dt nil :weight 'bold)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;org roam setup
(use-package org-roam
  :custom
  (org-roam-directory "~/projects/verdriet/notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)))

;org roam ui map
(use-package org-roam-ui
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start nil))

;Actual Org!
(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-support-shift-select 'always)
  (org-font-setup))

;custom functions for jinja template friendly html exports from org documents
;replace org text 'tags' to html divs
(defun my-replace-custom-tags-in-html-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (while (re-search-forward "\\(\\?note\\)\\|\\(\\?end\\)\\|\\(\\?highb\\)\\|\\(\\?high\\)" nil t)
      (cond
       ((match-string 1) ; ?NOTE
        (replace-match "<span class='margin-note'>"))
       ((match-string 2) ; ?END
        (replace-match "</span>"))
       ((match-string 3) ; ?HIGHB
        (replace-match "<span class='highlight_b'>"))
       ((match-string 4) ; ?HIGH
        (replace-match "<span class='highlight'>"))))
    (write-region (point-min) (point-max) file-path)))

;exports active org mode buffer to a temp buffer with the html parsed body, adds some jinja template text, then applies our regex replace for our keywords, and finally saves this back to a new file 'original buffer name'.html
(defun my-html-export ()
  (interactive)
  (let* ((export-file (concat (file-name-sans-extension (buffer-file-name)) ".html"))
         (html-content-buffer (org-export-to-buffer 'html "*My HTML Export*" nil nil t t))
         (html-content (with-current-buffer html-content-buffer (buffer-string))))
    (with-temp-buffer
      (insert "{% extends 'layout.html' %}\n{% block title %}\n<title>Title</title>\n{% endblock title %}\n\n{% block content %}\n<div class='article-title'><h1>Title</h1></div>\n<div class='article'>\n\n")
      (insert html-content)
      (insert "\n</div>\n{% endblock content %}")
      (write-region (point-min) (point-max) export-file))
    (my-replace-custom-tags-in-html-file export-file)
    (kill-buffer html-content-buffer)
    (delete-other-windows)
    (find-file export-file))
  (message "Org buffer exported to HTML!"))

;; Your existing key bindings (unchanged)
(my-leader
 "" '(:ignore t :which-key "Custom leader key with SPC")
 "." 'counsel-find-file
 "r" 'counsel-projectile-rg
 "b" '(hydra-buffers/body :which-key "Manage Buffers")
 "s" '(hydra-scale-text/body :which-key "Scale Text Size")
 "y" '(org-custom-html-highlight :which-key "Highlight website tags in org mode")
 "e" 'my-html-export)


;makes emacs look pretty
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-org-config))
(load-theme 'doom-tomorrow-night t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(golsp go-mode tree-sitter-langs tree-sitter which-key visual-fill-column use-package pyvenv python-mode org-roam-ui org-pdftools magit lsp-ivy hydra helpful general evil-nerd-commenter doom-themes counsel-projectile company-box all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

