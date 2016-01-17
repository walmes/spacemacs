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

(setq wz-ess-packages
  '(
    company
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-equals
    ))

(defun wz-ess/init-ess ()
  (use-package ess-site
    :mode (("\\.sp\\'"           . S-mode)
           ("/R/.*\\.q\\'"       . R-mode)
           ("\\.[qsS]\\'"        . S-mode)
           ("\\.[rR]\\'"         . R-mode)
           ("\\.[rR]nw\\'"       . Rnw-mode)
           ("\\.[sS]nw\\'"       . Snw-mode)
           ("\\.[rR]profile\\'"  . R-mode)
           ("NAMESPACE\\'"       . R-mode)
           ("CITATION\\'"        . R-mode)
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
    :commands (R)
    :init
    (progn
      (when (configuration-layer/package-usedp 'company)
        (add-hook 'ess-mode-hook 'company-mode))
      (add-hook 'ess-mode-hook
                '(lambda ()
                   (setq ess-indent-with-fancy-comments nil)
                   (setq-local comment-add 0)
                   ))
      ))

  ;; R -----------------------------------------------------------------
  (with-eval-after-load 'ess-site
    ;; Follow Hadley Wickham's R style guide
    (setq ess-first-continued-statement-offset 4
          ess-continued-statement-offset 0
          ess-expression-offset 4
          ess-nuke-trailing-whitespace-p t
          ess-default-style 'C++)
    (setq-default inferior-R-args
                  "--no-restore-history --no-save ")
    (setq comint-scroll-to-bottom-on-input t)
    (setq comint-scroll-to-bottom-on-output t)
    (setq comint-move-point-for-output t)
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
      "Prevent call ess-eval-buffer by accident, frequently by
       hitting C-c C-b instead of C-c C-n."
      (if (yes-or-no-p
           (format "Are you sure you want to evaluate the %s buffer?"
                   buffer-file-name))
          (message "ess-eval-buffer started.")
        (error "ess-eval-buffer canceled!")))

    (defun spacemacs/ess-start-repl ()
      "Start a REPL corresponding to the ess-language of the current buffer."
      (interactive)
      (cond ((string= "S" ess-language) (call-interactively 'R))))

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
    (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)
    ;; Movement across chunks in Rnw files.
    (global-set-key (kbd "C-S-<f5>") 'ess-eval-chunk)
    (global-set-key (kbd "C-S-<f6>") 'ess-eval-chunk-and-step)
    (global-set-key (kbd "C-S-<f7>") 'ess-noweb-next-code-chunk)
    (global-set-key (kbd "C-S-<f8>") 'ess-noweb-previous-code-chunk)
    (global-set-key (kbd "C-S-<f9>") 'ess-noweb-goto-chunk)
    ;; Easy navigation in s-exp.
    (global-set-key (kbd "<M-S-up>") 'backward-up-list)
    (global-set-key (kbd "<M-S-down>") 'down-list)
    (global-set-key (kbd "<M-right>") 'forward-sexp)
    (global-set-key (kbd "<M-left>") 'bakward-sexp)
    (global-set-key (kbd "<M-up>") 'backward-list)
    (global-set-key (kbd "<M-down>") 'forward-list)))

(defun wz-ess/init-ess-R-data-view ())

(defun wz-ess/init-ess-R-object-popup ())

(defun wz-ess/init-ess-smart-equals ()
  (use-package ess-smart-equals
    :defer t
    :if ess-enable-smart-equals
    :init
    (progn
      (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
      (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode))))
