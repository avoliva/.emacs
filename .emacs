;; Package lists should be added here
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This will automatically load packages which use use-package and aren't installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'all-the-icons)
  (package-refresh-contents)
  (package-install 'all-the-icons))
(require 'all-the-icons)

(use-package company
  :ensure t
  :init (global-company-mode)
  :hook (prog-mode . company-mode)
  :config
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

;; Optional, if you want prettier icons in the completion menu

(remove-hook 'company-mode-hook 'company-box-mode)

;; Configure ivy to use flx for fuzzy matching
(setq ivy-re-builders-alistx
      '((t . ivy--regex-fuzzy))) ; Use fuzzy matching everywhere

;; Install and configure counsel-projectile
(use-package counsel-projectile
  :ensure t
  :bind (("C-c p" . counsel-projectile-find-file)
         ("C-c f" . counsel-projectile-rg))
  :config
  (counsel-projectile-mode))

(use-package eslintd-fix
  :ensure t
  :hook ((js-mode typescript-mode) . eslintd-fix-mode))

;; Makes sure emacs is using shell $PATH
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x)) ;; Use NVM node
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "NVM_DIR"))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Install and configure flx
(use-package flx
  :ensure t)

(use-package helm
  :ensure t
  :config (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :after (projectile)
  :bind (("C-c p" . helm-projectile))
  :config
  (helm-projectile-on))

;; Sublime's goto anything recreation
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-c s" . swiper))
  :config
  (ivy-mode 1))

;; LSP and autocompletion stuff
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred))
  :config
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-modeline-diagnostics-enable nil))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c d" . mc/mark-next-like-this)
         ("C-c k" . mc/skip-to-next-like-this)
	 ("C-c C-d" . mc/mark-all-like-this)))

(use-package prettier-js
  :ensure t
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (projectile-mode +1))


(use-package treemacs
  :ensure t
  :bind (("C-x t t" . treemacs)
         ("C-x t p" . treemacs-projectile)
         ("C-x t a" . treemacs-add-and-display-current-project))
  :config
  (setq treemacs-is-never-other-window t))
(setq treemacs-icon-set 'default)


(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(setq js-indent-level 2)


(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(setq lsp-eslint-server-command
      '("node"
        "/home/avoliva/.nvm/versions/node/v18.15.0/lib/node_modules/typescript-language-server/lib/cli.js"
        "--stdio"))
;; Set to maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Don't touch this stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c2efe6f5e2bd0bddfb2d6e26040545768939d2029f77e6b6a18d1ee0e0cb1033" "2a7d6f6a46b1b8504b269029a7375597f208a5cdfa1ea125b09255a592bb326e" "e16cd3e6093cf35e65dbac6c7649931936f32f56be34477cb7cbe1ee332c5b99" "72cc2c6c5642b117034b99dcc3a33ff97a66593429c7f44cd21b995b17eebd4e" "ffef467dfed832df46d4e188049e52aad1d64c16070484fc6b62f158ece95471" "f48be80177f0d9a2b19d8dc19f3903d9be3c4d885d110e82b591d1184586fad0" "f681100b27d783fefc3b62f44f84eb7fa0ce73ec183ebea5903df506eb314077" "fd029ad4c1213f32dbd50acfd4aead9aafc7b62d00c5bc6237ccb2bc028fabd1" "6198e96f1fd7de3889a1b6ab8be1fc9b7c734cc9db0b0f16b635a2974601f977" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(ef-themes eslintd-fix prettier rg counsel-projectile counsel ag all-the-icons prettier-js flycheck yasnippet evil-visual-mark-mode bubbleberry-theme dracula-theme which-key treemacs-projectile treemacs multiple-cursors company-box exec-path-from-shell company helm-projectile helm projectile magit)))


;;Reload this file
(defun my-reload-init-file ()
  "Reload the Emacs init file."
  (interactive)
  (load-file "~/.emacs"))

(global-set-key (kbd "C-c e") 'my-reload-init-file)

;; Custom theme
(load-theme 'bubbleberry t)
(scroll-bar-mode -1)
(menu-bar-mode -1)


(global-display-line-numbers-mode 1) ; For displaying line numbers
(defun my-disable-line-numbers-in-treemacs ()
  "Disable line numbers in Treemacs buffers."
  (display-line-numbers-mode -1))

(add-hook 'treemacs-mode-hook #'my-disable-line-numbers-in-treemacs)


;; Change goto-line to be C-:
(global-set-key (kbd "C-:") 'goto-line)

;; Opens a shell
(defun my/open-shell-in-bottom ()
  "Open a shell in a window at the bottom of the frame."
  (interactive)
  (let ((buffer (shell)))
    (switch-to-buffer (other-buffer buffer))
    (split-window-below)
    (windmove-down)
    (switch-to-buffer buffer)))


(global-set-key (kbd "C-c s") #'my/open-shell-in-bottom)

;; Closes the current active window
(defun my/close-active-window ()
  "Close the active window and kill its buffer without prompting."
  (interactive)
  (let ((current-buffer (current-buffer))
        (kill-buffer-query-functions ; Bypass process exit confirmation
         (remq 'process-kill-buffer-query-function kill-buffer-query-functions)))
    (delete-window (selected-window))
    (kill-buffer current-buffer)))

(global-set-key (kbd "C-c w") #'my/close-active-window);; Set default frame setting


;; Define a function to create a four-window layout
(defun my-startup-layout ()
  "Set up the startup layout."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 2)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (other-window 1)
  (switch-to-buffer "*Messages*")
  (other-window 1)
  (switch-to-buffer (other-buffer))
  (balance-windows))

;; Going to try removing this to see if it helps with desktop restoration
(add-hook 'emacs-startup-hook 'my-startup-layout)

; (desktop-save-mode 1)
; (setq desktop-restore-frames t) ; Restore frame configuration
; (setq desktop-load-locked-desktop t) ; Load the desktop even if it is locked

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(setq initial-buffer-choice t)



;; Set node version
(setq my-nvm-node-path "~/.nvm/versions/node/v18.15.0/bin/node")
(if (file-exists-p my-nvm-node-path)
    (progn
      (setq my-nvm-bin-path (file-name-directory my-nvm-node-path))
      (setenv "PATH" (concat my-nvm-bin-path ":" (getenv "PATH")))
      (add-to-list 'exec-path my-nvm-bin-path))
  (message "Warning: nvm node not found at %s" my-nvm-node-path))

(defun my/use-local-eslint ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-local-prettier ()
  "Use local prettier from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                        (expand-file-name "node_modules/.bin/prettier"
                                          root))))
    (when (and prettier (file-executable-p prettier))
      (setq-local prettier-js-command prettier))))

(add-hook 'js-mode-hook #'my/use-local-eslint)
(add-hook 'typescript-mode-hook #'my/use-local-eslint)
(add-hook 'js-mode-hook #'my/use-local-prettier)
(add-hook 'typescript-mode-hook #'my/use-local-prettier)

;; Tab to spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Overwrite C-z
(global-set-key (kbd "C-z") 'undo)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;;; .emacs ends here
