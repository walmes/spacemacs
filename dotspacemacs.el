;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
   You should not put any user code in this function besides
   modifying the variable values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     markdown
     latex
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     wz-ess
     wz-misc
     wz-polymode
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(keyring
                                    rainbow-delimiters)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
   This function is called at the very startup of Spacemacs
   initialization before layers configuration. You should not put
   any user code in there besides modifying the variable values."
  (setq-default
   dotspacemacs-elpa-https                t
   dotspacemacs-elpa-timeout              5
   dotspacemacs-check-for-update          t
   dotspacemacs-editing-style             'emacs
   dotspacemacs-verbose-loading           nil
   dotspacemacs-startup-banner            'official
   dotspacemacs-startup-lists             '(recents projects)
   dotspacemacs-startup-recent-list-size  5
   dotspacemacs-scratch-mode              'lisp-mode
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-active-transparency          90
   dotspacemacs-auto-resume-layouts          nil
   dotspacemacs-auto-save-file-location      'cache
   dotspacemacs-command-key                  ":"
   dotspacemacs-default-layout-name          "Default"
   dotspacemacs-default-package-repository   nil
   dotspacemacs-display-default-layout       nil
   dotspacemacs-distinguish-gui-tab          nil
   dotspacemacs-emacs-leader-key             "M-m"
   dotspacemacs-enable-paste-micro-state     nil
   dotspacemacs-fullscreen-at-startup        nil
   dotspacemacs-fullscreen-use-non-native    nil
   dotspacemacs-helm-no-header               nil
   dotspacemacs-helm-position                'bottom
   dotspacemacs-helm-resize                  nil
   dotspacemacs-highlight-delimiters         'all
   dotspacemacs-inactive-transparency        90
   dotspacemacs-leader-key                   "SPC"
   dotspacemacs-line-numbers                 nil
   dotspacemacs-loading-progress-bar         t
   dotspacemacs-major-mode-emacs-leader-key  "C-M-m"
   dotspacemacs-major-mode-leader-key        ","
   dotspacemacs-max-rollback-slots           5
   dotspacemacs-maximized-at-startup         nil
   dotspacemacs-mode-line-unicode-symbols    nil
   dotspacemacs-persistent-server            nil
   dotspacemacs-remap-Y-to-y$                t
   dotspacemacs-search-tools                 '("ag" "pt" "ack" "grep")
   dotspacemacs-smartparens-strict-mode      nil
   dotspacemacs-smooth-scrolling             t
   dotspacemacs-use-ido                      nil
   dotspacemacs-which-key-delay              0.4
   dotspacemacs-which-key-position           'bottom
   dotspacemacs-whitespace-cleanup           nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
   It is called immediately after `dotspacemacs/init'. You are
   free to put almost any user code here. The exception is org
   related code, which should be placed in
   `dotspacemacs/user-config'."
  ;;-------------------------------------------
  (setq user-full-name "Walmes Zeviani"
        user-mail-address "walmes@ufpr.com")
  ;;-------------------------------------------
  ;; Font type and size.
  (cond
   ((string-equal system-name "brother")
    (set-default-font "Ubuntu Mono-16"))
   ((string-equal system-name "youngest")
    (set-default-font "Ubuntu Mono-16"))
   ((string-equal system-name "first")
    (set-default-font "Ubuntu Mono-14"))
   ((string-equal system-name "class")
    (set-default-font "Ubuntu Mono-14")))
  ;;-------------------------------------------
  ;; Font for especial words.
  (make-face 'special-words)
  (set-face-attribute 'special-words nil
                      :foreground "White"
                      :background "Firebrick")
  (dolist (mode '(fundamental-mode
                  gnus-article-mode
                  lisp-mode
                  org-mode
                  shell-mode
                  sh-mode
                  muse-mode
                  ess-mode
                  polymode-mode
                  markdown-mode
                  latex-mode
                  TeX-mode))
    (font-lock-add-keywords
     mode
     '(("\\<\\(IMPORTANT\\|ATTENTION\\|NOTE\\|TODO\\|DONE\\|STOP\\)"
        0 'font-lock-warning-face t)
       ("\\<\\(COMMENT\\|IMPROVE\\|REVIEW\\)"
        0 'font-lock-warning-face t)
       ("\\<\\(BUG\\|WARNING\\|DANGER\\|FIXME\\)"
        0 'special-words t))))
  ) ;; dotspacemacs/user-init

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
   This function is called at the very end of Spacemacs
   initialization after layers configuration. You are free to put
   any user code."
  ;;-------------------------------------------
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (setq comment-empty-lines t)    ;; comments in empty lines.
  (setq auto-save-default nil)    ;; no #autosave#.
  (setq make-backup-files nil)    ;; no backup~.
  (setq inhibit-startup-screen t) ;; no startup
  (delete-selection-mode 1)
  (auto-fill-mode t)
  (setq-default fill-column 72)
  (setq-default auto-fill-function 'do-auto-fill)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq whitespace-line-column fill-column)
  (setq whitespace-style '(face lines-tail trailing spaces tabs empty))
  (global-whitespace-mode +1)
  ;;-------------------------------------------
  (global-set-key [(control tab)] 'other-window)
  (global-set-key (kbd "C-<next>") 'next-buffer)
  (global-set-key (kbd "C-<prior>") 'previous-buffer)
  (global-unset-key "\C-z")     ;; 'undo default is C-/.
  (global-set-key "\C-z" 'undo) ;; C-z to 'undo;
  ;; M-. to (un)comment paragraph.
  (global-set-key [?\M-.] (kbd "M-h M-; M-}"))
  ;; "C-~" to keep one white space between objects around point.
  (global-set-key (kbd "<C-dead-tilde>") 'fixup-whitespace)
  ;; "M-~" to joint lines.
  (global-set-key (kbd "<M-dead-tilde>") 'delete-indentation)
  ;;-------------------------------------------
  (add-hook 'emacs-startup-hook 'delete-other-windows)[/code]
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;;-------------------------------------------
  ;; org and babel
  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((R . t)
                                   (emacs-lisp . t)
                                   (sh . t)
                                   (python . t)))
    (setq org-confirm-babel-evaluate nil)
    (setq org-replace-disputed-keys t)
    (setq org-return-follows-link t)
    (setq org-src-fontify-natively t) ;; fontify code in code blocks.
    (setq org-latex-listings t)
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color")))
  ) ;; dotspacemacs/user-config

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
