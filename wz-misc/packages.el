(setq wz-misc-packages
  '(
    bm
    yafolding
    bookmark+
    smartparens
    (electric-spacing :location (recipe
                                 :fetcher file
                                 :path "~/GitLab/electric-spacing/electric-spacing.el"))
    ))

(defun wz-misc/init-bm ()
  (use-package bm)
  (with-eval-after-load 'bm
    (setq bm-marker 'bm-marker-left)
    (setq bm-highlight-style 'bm-highlight-only-fringe)
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

(defun wz-misc/init-bookmark+ ()
  (use-package bookmark+
    :init
    (progn
      (setq bookmark-default-file "~/Dropbox/bookmarks"
            bookmark-save-flag 1))
    :config
    (progn
      ;; Create an autonamed bookmark.
      (global-set-key
       (kbd "<C-f3>") 'bmkp-toggle-autonamed-bookmark-set/delete)
      ;; Go to the next bookmark in file.
      (global-set-key
       (kbd "<f3>") 'bmkp-next-bookmark-this-file/buffer-repeat)
      ;; Go to the previous bookmark in file.
      (global-set-key
       (kbd "<f4>") 'bmkp-previous-bookmark-this-file/buffer-repeat)
      ;; Toggle temporary/permanent bookmark.
      (global-set-key
       (kbd "<S-f3>") 'bmkp-toggle-temporary-bookmark))
    )
  ) ;; wz-misc/init-bookmark+

(defun wz-misc/init-smartparens ()
  (smartparens-global-mode 1)
  (sp-pair "\"" nil :unless '(sp-point-after-word-p))
  (sp-pair "'" nil :unless '(sp-point-after-word-p)))

(defun wz-misc/init-electric-spacing ()
  (use-package electric-spacing
    :config
    (progn
      (add-hook 'ess-mode-hook #'electric-spacing-mode))))
