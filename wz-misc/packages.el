(setq wz-misc-packages
  '(
    bm
    ))

(defun wz-misc/init-bm ()
  (use-package bm
    :init
    (progn
      (setq bm-marker 'bm-marker-left)
      (setq bm-highlight-style 'bm-highlight-only-fringe)
      ))

  (with-eval-after-load 'bm
    (global-set-key (kbd "<C-f2>") 'bm-toggle)
    (global-set-key (kbd "<f2>") 'bm-next)
    (global-set-key (kbd "<S-f2>") 'bm-previous)
    )
  ) ;; wz-misc/init-bm
