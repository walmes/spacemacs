(setq wz-misc-packages
  '(
    bm
    yafolding
    bookmark+
    auto-complete
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

(defun auto-completion/post-init-auto-complete ()
  (use-package auto-complete
    :defer t
    :init
    (setq ac-auto-start 0
          ac-delay 0.2
          ac-quick-help-delay 1.
          ac-use-fuzzy t
          ac-fuzzy-enable t
          ac-comphist-file (concat
                            spacemacs-cache-directory "ac-comphist.dat")
          ;; use 'complete when auto-complete is disabled
          tab-always-indent 'complete
          ac-dwim t)
    :config
    (progn
      (require 'auto-complete-config)
      (ac-config-default)
      (setq-default ac-sources '(ac-source-abbrev
                                 ac-source-dictionary
                                 ac-source-words-in-same-mode-buffers))
      (add-to-list 'completion-styles 'initials t)
      ;; Change 'ac-complete from ENTER to TAB.
      (define-key ac-completing-map "\r" nil)
      (define-key ac-completing-map "\t" 'ac-complete)
      (spacemacs|diminish auto-complete-mode " ⓐ" " a"))))
