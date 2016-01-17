(setq wz-polymode-packages
  '(
    polymode
    ))

(defun wz-polymode/init-polymode ()
  (use-package polymode
    :mode (("\\.Rmd"   . rmd-mode)
           ("\\.Rpres" . rmd-mode))
    :init
    (progn
      (defun rmd-mode ()
        "ESS Markdown mode for rmd files"
        (interactive)
        (require 'poly-R)
        (require 'poly-markdown)
        (R-mode)
        (poly-markdown+r-mode))
      ))

  (with-eval-after-load 'rmd-mode
    (global-set-key (kbd "S-<f7>") 'polymode-previous-chunk)
    (global-set-key (kbd "S-<f8>") 'polymode-next-chunk)
    (global-set-key (kbd "S-<f9>") 'polymode-insert-new-chunk))
  ) ;; wz-misc/init-polymode
