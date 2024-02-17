;;; packages.el --- ESS (R) Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ess-packages
  '(
    ;; company
    ess
    ess-R-data-view
    ;; ess-R-object-popup
    ess-smart-equals
    rainbow-delimiters
    (electric-spacing-r :location local)
    ;; (electric-spacing-r :location
    ;;                     (recipe :fetcher github
    ;;                             :repo "walmes/electric-spacing"
    ;;                             :files ("electric-spacing-r.el")))
    ;; (electric-spacing-r :location
    ;;                     (recipe :url "https://raw.githubusercontent.com/walmes/electric-spacing/master/electric-spacing-r.el"))
    ;; NOTE: Compilation ALWAYS fails when fetching from URL or Github.
    (essh :location local)
    ;; (essh :location
    ;;       (recipe :url "https://www.emacswiki.org/emacs/download/essh.el"))
    ;; NOTE: Compilation ALWAYS fails when fetching from URL or Github.
    smartparens
    ;; :local electric-spacing-r
    ))

(defun ess/init-ess ()
  (use-package ess-site
    :mode (("\\.sp\\'"           . S-mode)
           ("/R/.*\\.q\\'"       . R-mode)
           ("\\.[qsS]\\'"        . S-mode)
           ("\\.ssc\\'"          . S-mode)
           ("\\.SSC\\'"          . S-mode)
           ("\\.[rR]\\'"         . R-mode)
           ("\\.[rR]nw\\'"       . Rnw-mode)
           ("\\.[sS]nw\\'"       . Snw-mode)
           ("\\.[rR]profile\\'"  . R-mode)
           ("NAMESPACE\\'"       . R-mode)
           ("CITATION\\'"        . R-mode)
           ("\\.omg\\'"          . omegahat-mode)
           ("\\.hat\\'"          . omegahat-mode)
           ("\\.lsp\\'"          . XLS-mode)
           ("\\.do\\'"           . STA-mode)
           ("\\.ado\\'"          . STA-mode)
           ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
           ("\\.jl\\'"           . ess-julia-mode)
           ("\\.[Ss]t\\'"        . S-transcript-mode)
           ("\\.Sout"            . S-transcript-mode)
           ("\\.[Rr]out"         . R-transcript-mode)
           ("\\.Rd\\'"           . Rd-mode)
           ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
           ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
           ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
           ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
           ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
           ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
    :commands (R stata julia SAS)
    :init
    (progn
      (when (configuration-layer/package-usedp 'company)
        (add-hook 'ess-mode-hook 'company-mode))
      (setq ess-indent-with-fancy-comments nil
            comint-scroll-to-bottom-on-input t
            comint-scroll-to-bottom-on-output t
            comint-move-point-for-output t
            ess-indent-offset 4)
      )
    (use-package ess-r-mode
      :bind
      (:map ess-r-mode-map
            ;; ("C-|" . " |>")
            ("M--" . ess-insert-assign)
            ("M-p" . add-pipe-magrittr)
            ("C-|" . add-pipe-native)
            ("M-o" . add-pipe-native)
            ("M-k" . add-knitr-opts))
      (:map inferior-ess-r-mode-map
            ("M--" . ess-insert-assign)
            ("M-p" . add-pipe-magrittr-inf)
            ("qC-|" . add-pipe-native-inf)
            ("M-o" . add-pipe-native-inf))
      :config
      (defun add-pipe-magrittr ()
        (interactive)
        (end-of-line)
        (unless (looking-back "%>%" nil)
          (just-one-space 1)
          (insert "%>%"))
        (newline-and-indent))
      (defun add-pipe-magrittr-inf ()
        (interactive)
        (end-of-line)
        (unless (looking-back "%>%" nil)
          (just-one-space 1)
          (insert "%>% ")))
      (defun add-pipe-native ()
        (interactive)
        (end-of-line)
        (unless (looking-back "|>" nil)
          (just-one-space 1)
          (insert "|>"))
        (newline-and-indent))
      (defun add-pipe-native-inf ()
        (interactive)
        (end-of-line)
        (unless (looking-back "|>" nil)
          (just-one-space 1)
          (insert "|> ")))
      (defun add-knitr-opts ()
        (interactive)
        (beginning-of-line)
        (unless (looking-back "#|" nil)
          (just-one-space 0)
          (insert "#| ")
          (end-of-line)))
      )
    )

  (use-package ess
    :bind
    ("M-="    . ess-cycle-assign)     ;; `Alt + -'  to cycle ` <- | <<- | = ...'.
    ("S-<f5>" . company-R-args)
    ("C-<f5>" . company-R-objects)
    ("<f8>"   . lsp-ui-imenu)
    ("C-<f8>" . lsp-treemacs-symbols)
    ("<f9>"   . treemacs)
    ("<f10>"  . imenu-list)
    ;; ("C-<f9>" . lsp-treemacs-errors-list)
    ;; ("C-<f10>" . lsp-treemacs-references)
    ;; ("C-<f11>" . lsp-treemacs-implementations)
    ;; ("C-<f12>" . lsp-treemacs-call-hierarchy)
    :init
    (progn
      (message "use-package ess :init")
      (setq-default ess-dialect "R")
      (setq-default inferior-R-args "--no-restore-history --no-save ")
      ;; (setq inferior-ess-r-program "/home/walmes/anaconda3/bin/R")
      (setq ess-indent-with-fancy-comments nil
            comint-scroll-to-bottom-on-input t
            comint-scroll-to-bottom-on-output t
            comint-move-point-for-output t
            ess-indent-offset 4)
      (setq-local comment-add 0) ;; Single # as default.
      ;; (setq lsp-diagnostics-provider :none)
      )
    :hook
    (ess-mode . (lambda ()
                  (yas-minor-mode)   ;; Enable yasnippet.
                  (flycheck-mode -1) ;; Disable flycheck/lintr.
                  (lsp)              ;; Enable LSP.
                  (setq lsp-diagnostics-provider :none) ;; Disable flycheck.
                  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
                  (lsp-ui-doc-use-webkit t)
                  (setq-local comment-add 0) ;; Single # as default.
                  (setq ess-r-backend 'lsp
                        ess-style 'RStudio
                        ess-use-flymake nil
                        ess-smart-operators t)
                  ))
    :config
    (setq ess-R-font-lock-keywords
          '((ess-R-fl-keyword:modifiers . t)
            (ess-R-fl-keyword:fun-defs . t)
            (ess-R-fl-keyword:keywords . t)
            (ess-R-fl-keyword:assign-ops . t)
            (ess-R-fl-keyword:constants . t)
            (ess-fl-keyword:fun-calls . t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators . t)
            (ess-fl-keyword:delimiters . t)
            (ess-fl-keyword:= . t)
            (ess-R-fl-keyword:F&T . t)))
    (setq inferior-R-font-lock-keywords
          '((ess-S-fl-keyword:prompt . t)
            (ess-R-fl-keyword:messages . t)
            (ess-R-fl-keyword:modifiers . t)
            (ess-R-fl-keyword:fun-defs . t)
            (ess-R-fl-keyword:keywords . t)
            (ess-R-fl-keyword:assign-ops . t)
            (ess-R-fl-keyword:constants . t)
            (ess-fl-keyword:matrix-labels . t)
            (ess-fl-keyword:fun-calls . t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators . t)
            (ess-fl-keyword:delimiters . t)
            (ess-fl-keyword:= . t)
            (ess-R-fl-keyword:F&T . t)))
    (defadvice ess-eval-buffer (before really-eval-buffer compile activate)
      "Prevent call ess-eval-buffer by accident,
                 frequently by hitting C-c C-b instead of C-c C-n."
      (if (yes-or-no-p
           (format "Are you sure you want to evaluate the %s buffer?"
                   buffer-file-name))
          (message "ess-eval-buffer started.")
        (error "ess-eval-buffer canceled!")))
    )

  ;; R --------------------------------------------------------------------------
  (with-eval-after-load 'ess-site
    (setq ess-nuke-trailing-whitespace-p 'ask)
    ;; or even
    ;; (setq ess-nuke-trailing-whitespace-p t)
    ;; Perl
    (add-hook 'perl-mode-hook
              (lambda () (setq perl-indent-level 4)))

    (defun spacemacs/ess-start-repl ()
      "Start a REPL corresponding to the ess-language of the current buffer."
      (interactive)
      (cond
       ((string= "S" ess-language) (call-interactively 'R))
       ((string= "STA" ess-language) (call-interactively 'stata))
       ((string= "SAS" ess-language) (call-interactively 'SAS))))

    (spacemacs/set-leader-keys-for-major-mode 'ess-julia-mode
      "si" 'julia)
    (spacemacs/set-leader-keys-for-major-mode 'ess-mode
      "si" 'spacemacs/ess-start-repl
      ;; noweb
      "cC" 'ess-eval-chunk-and-go
      "cc" 'ess-eval-chunk
      "cd" 'ess-eval-chunk-and-step
      "cm" 'ess-noweb-mark-chunk
      "cN" 'ess-noweb-previous-chunk
      "cn" 'ess-noweb-next-chunk
      ;; REPL
      "sB" 'ess-eval-buffer-and-go
      "sb" 'ess-eval-buffer
      "sD" 'ess-eval-function-or-paragraph-and-step
      "sd" 'ess-eval-region-or-line-and-step
      "sL" 'ess-eval-line-and-go
      "sl" 'ess-eval-line
      "sR" 'ess-eval-region-and-go
      "sr" 'ess-eval-region
      "sT" 'ess-eval-function-and-go
      "st" 'ess-eval-function
      ;; R helpers
      "hd" 'ess-R-dv-pprint
      "hi" 'ess-R-object-popup
      "ht" 'ess-R-dv-ctable
      )
    (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
    (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
    (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)))

(defun ess/init-ess-R-data-view ())

(defun ess/init-ess-R-object-popup ())

(defun ess/post-init-rainbow-delimiters ()
  (add-hook 'ess-mode-hook #'rainbow-delimiters-mode))

;; To enable smart-equals-mode
(defun ess/init-ess-smart-equals ()
  (use-package ess-smart-equals
    :defer t
    :if ess-enable-smart-equals
    :init
    ;; (setq ess-smart-equals-extra-ops '(brace paren percent))
    (progn
      (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
      (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode))))

;; To enable smartparens-mode in ess and iess
(defun ess/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :if ess-enable-smartparens
    :diminish smartparens-mode
    :config
    (progn
      (require 'smartparens-config)
      (smartparens-global-mode 1)
      (sp-pair "\"" nil :unless '(sp-point-after-word-p))
      (sp-pair "'" nil :unless '(sp-point-after-word-p))
      (sp-local-pair '(ess-mode inferior-ess-mode) "(" nil :unless '(sp-point-before-word-p))
      (sp-local-pair '(ess-mode inferior-ess-mode) "[" nil :unless '(sp-point-before-word-p))
      (sp-local-pair '(ess-mode inferior-ess-mode) "{" nil :unless '(sp-point-before-word-p)))
    :init
    (progn
      (add-hook 'ess-mode-hook 'smartparens-mode)
      (add-hook 'inferior-ess-mode-hook 'smartparens-mode))))

;; To enable electric-spacing-mode in ess and iess
(defun ess/init-electric-spacing-r ()
  (use-package electric-spacing-r
    ;; :defer t
    :if ess-enable-electric-spacing-r
    :config
    (spacemacs|diminish electric-spacing-mode "⚡" "_+_")
    :init
    (progn
      (add-hook 'ess-mode-hook 'electric-spacing-mode))))
      ;; (add-hook 'inferior-ess-mode-hook 'electric-spacing-mode))))

(defun ess/init-essh ()
  (use-package essh
    :config
    (progn
      (add-hook
       'sh-mode-hook
       #'(lambda ()
          (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
          (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
          (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
          ;; (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
          (define-key sh-mode-map (kbd "<C-return>") 'pipe-line-to-shell-and-step)
          (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
          (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))))))
