(setq wz-misc-packages
  '(
    bm
    yafolding
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
    (global-set-key (kbd "<S-f2>") 'bm-previous))
  ) ;; wz-misc/init-bm

(defun wz-misc/init-yafolding ()
  (use-package yafolding
    :init
    (progn
      (yafolding-mode))
    :config
    (progn
      (global-set-key [?\C-{] #'yafolding-hide-parent-element)
      (global-set-key [?\C-}] #'yafolding-toggle-element)))
  ) ;; wz-misc/init-yafolding
