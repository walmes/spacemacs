(setq wz-misc-packages
  '(
    bm
    yafolding
    bookmark+
    smartparens
    (electric-spacing :location (recipe
                                 :fetcher github
                                 :repo "walmes/electric-spacing")
                      :upgrade nil)
    (essh :location (recipe
                     :fetcher file
                     :path "~/.emacs.d/private/wz-misc/essh/essh.el")
          :upgrade nil)
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
  (use-package electric-spacing)
  (add-hook 'ess-mode-hook #'electric-spacing-mode))

(defun wz-misc/init-essh ()
  (use-package essh
    :config
    (progn
      (add-hook
       'sh-mode-hook
       '(lambda ()
          (define-key sh-mode-map "\C-c\C-r"
            'pipe-region-to-shell)
          (define-key sh-mode-map "\C-c\C-b"
            'pipe-buffer-to-shell)
          (define-key sh-mode-map "\C-c\C-j"
            'pipe-line-to-shell)
          (define-key sh-mode-map "\C-c\C-n"
            'pipe-line-to-shell-and-step)
          (define-key sh-mode-map "\C-c\C-f"
            'pipe-function-to-shell)
          (define-key sh-mode-map "\C-c\C-d"
            'shell-cd-current-directory))))
    )
  ) ;; wz-misc/init-essh
