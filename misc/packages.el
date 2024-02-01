(setq misc-packages
  '(
    yafolding
    yasnippet ;; https://develop.spacemacs.org/layers/LAYERS.html#auto-completion
    hl-todo   ;; https://develop.spacemacs.org/layers/LAYERS.html#spacemacs-visual
    bm        ;; https://develop.spacemacs.org/layers/LAYERS.html#bm
    ;; visual-fill-column
    ;; (hi-lock :location local)
    ;; hl-prog-extra
    ;; lsp-ui       ;; layer lsp?.
    ;; lsp-treemacs ;; layer lsp?.
    ;; treemacs     ;; layer filetree.
    (screenshot
     :location (recipe
                :fetcher github
                :repo "tecosaur/screenshot"))
    (bookmark+
     :location (recipe
                :fetcher github
                :repo "emacsmirror/bookmark-plus"))
    ;; (copilot
    ;;  :location (recipe
    ;;             :fetcher github
    ;;             :repo "zerolfx/copilot.el"
    ;;             :files ("*.el" "dist")))
    )
  )

(defun misc/init-yafolding ()
    :defer t
    :init
    (yafolding-mode)
    :config
    (progn
      ;; Create an autonamed bookmark.
      (global-set-key (kbd "C-{")
                      'yafolding-hide-parent-element)
      ;; Go to the next bookmark in file.
      (global-set-key (kbd "C-}")
                      'yafolding-toggle-element)
      (spacemacs|diminish yas-minor-mode "🅨" "{}")
      )
    ;; :bind
    ;; (("C-{" . yafolding-hide-parent-element)
    ;;  ("C-}" . yafolding-toggle-element))
    )

(defun misc/init-yasnippet ()
  (use-package yasnippet
    :defer t
    :config
    (yas-global-mode 1)
    (spacemacs|diminish yafolding-mode "🅨" "yas")
    )
  )
;; (defun misc/post-init-yasnippet ()
;;   ;; Configurações para o pacote yasnippet, se necessário.
;;   ;; Por exemplo, adicionar diretórios de snippets personalizados:
;;   (add-to-list 'yas-snippet-dirs "~/.emacs.d/private/sua-camada/snippets"))

(defface hl-todo-caution-words
  '((t :foreground "OrangeRed"
       :background "LightGray"
       :inherit (hl-todo)))
  "Face for highlighting the CAUTION keyword.")

(defface hl-todo-good-words
  '((t :foreground "LightSeaGreen"
       :background "White"
       :inherit (hl-todo)))
  "Face for highlighting the GOOD/POSITIVE keyword.")

(defface hl-todo-bad-words
  '((t :foreground "White"
       :background "Firebrick"
       :inherit (hl-todo)))
  "Face for highlighting the BAD/NEGATIVE keyword.")

(defun misc/init-hl-todo ()
  (use-package hl-todo
    :defer t
    :hook (prog-mode . hl-todo-mode)
    :bind
    ("C-c l m" . hl-todo-previous)
    ("C-c l n" . hl-todo-next)
    :config
    (add-to-list 'hl-todo-keyword-faces '("IMPROVE"     font-lock-constant-face bold))
    (add-to-list 'hl-todo-keyword-faces '("QUESTION"    font-lock-constant-face bold))
    (add-to-list 'hl-todo-keyword-faces '("EXPLANATION" font-lock-constant-face bold))
    (add-to-list 'hl-todo-keyword-faces '("THEORY"      font-lock-constant-face bold))
    (add-to-list 'hl-todo-keyword-faces '("DESCRIPTION" font-lock-keyword-face bold))
    (add-to-list 'hl-todo-keyword-faces '("COMMENT"     font-lock-keyword-face bold))
    (add-to-list 'hl-todo-keyword-faces '("TIP"         font-lock-keyword-face bold))
    (add-to-list 'hl-todo-keyword-faces '("TRICK"       font-lock-keyword-face bold))
    (add-to-list 'hl-todo-keyword-faces '("DANGER"      error bold))
    (add-to-list 'hl-todo-keyword-faces '("STOP"        error bold))
    (add-to-list 'hl-todo-keyword-faces '("FAIL"        error bold))
    (add-to-list 'hl-todo-keyword-faces '("WARNING"     error bold))
    (add-to-list 'hl-todo-keyword-faces '("ERROR"       error bold))
    (add-to-list 'hl-todo-keyword-faces '("BUG"         error bold))
    (add-to-list 'hl-todo-keyword-faces '("DEBUG"       warning bold))
    (add-to-list 'hl-todo-keyword-faces '("IMPORTANT"   warning bold))
    (add-to-list 'hl-todo-keyword-faces '("ATTENTION"   warning bold))
    (add-to-list 'hl-todo-keyword-faces '("CAUTION"     warning bold))
    (add-to-list 'hl-todo-keyword-faces '("OBS"         warning bold))
    (add-to-list 'hl-todo-keyword-faces '("PROBLEM"     warning bold))
    (add-to-list 'hl-todo-keyword-faces '("DISCLAIMER"  warning bold))
    (add-to-list 'hl-todo-keyword-faces '("EXERCISE"    warning bold))
    (add-to-list 'hl-todo-keyword-faces '("BONUS"       success bold))
    (add-to-list 'hl-todo-keyword-faces '("DONE"        success bold))
    (add-to-list 'hl-todo-keyword-faces '("OKAY"        success bold))
    (add-to-list 'hl-todo-keyword-faces '("GOOD"        success bold))
    (add-to-list 'hl-todo-keyword-faces '("SOLVED"      success bold))
    (add-to-list 'hl-todo-keyword-faces '("OPTIONAL"    success bold))
    (add-to-list 'hl-todo-keyword-faces '("WALMES"    . hl-todo-good-words))
    )
  )

(defun misc/init-bm ()
  (use-package bm
    :defer t
    :config
    (setq bm-marker 'bm-marker-left
          bm-highlight-style 'bm-highlight-only-fringe)
    :bind
    (("<C-f2>" . bm-toggle)
     ("<f2>"   . bm-next)
     ("<S-f2>" . bm-previous))
    )
  )

(defun misc/init-screenshot ()
  (use-package screenshot
    :defer t
    :config
    (setq screenshot-schemes
          '(("current-directory" :dir default-directory)))
    (setq screenshot-default-scheme "current-directory")
    )
  )

(defun misc/init-bookmark+ ()
  (use-package bookmark+
    :defer t
    :init
    (when (file-exists-p "~/Dropbox/bookmarks")
      (setq bookmark-default-file "~/Dropbox/bookmarks"
            bookmark-save-flag 1))
    ;; :config
    ;; ;; ATTENTION: for some unknown reason, the keymap must be defined in
    ;; ;; `:config' because in `:bind' the bookmark list buffer have a
    ;; ;; different appearance.
    ;; (progn
    ;;   ;; Create an autonamed bookmark.
    ;;   (global-set-key (kbd "<C-f3>")
    ;;                   'bmkp-toggle-autonamed-bookmark-set/delete)
    ;;   ;; Go to the next bookmark in file.
    ;;   (global-set-key (kbd "<f3>")
    ;;                   'bmkp-next-bookmark-this-file/buffer-repeat)
    ;;   ;; Go to the previous bookmark in file.
    ;;   (global-set-key (kbd "<f4>")
    ;;                   'bmkp-previous-bookmark-this-file/buffer-repeat)
    ;;   ;; Toggle temporary/permanent bookmark.
    ;;   (global-set-key (kbd "<S-f3>")
    ;;                   'bmkp-toggle-temporary-bookmark)
    ;;   )
    :bind
    (("<C-f3>" . bmkp-toggle-autonamed-bookmark-set/delete)
     ("<f3>"   . bmkp-next-bookmark-this-file/buffer-repeat)
     ("<f4>"   . bmkp-previous-bookmark-this-file/buffer-repeat)
     ("<S-f3>" . bmkp-toggle-temporary-bookmark))
    ;; :config
    ;; (spacemacs|diminish ?? "Ⓑ" "B+")
    )
  )

;; (defun misc/init-copilot ()
;;   (use-package copilot
;;     :defer t
;;     :hook (prog-mode . copilot-mode)
;;     :bind (:map copilot-completion-map
;;                 ("<tab>" . 'copilot-accept-completion)
;;                 ("TAB" . 'copilot-accept-completion)
;;                 ("C-TAB" . 'copilot-accept-completion-by-word)
;;                 ("C-<tab>" . 'copilot-accept-completion-by-word))
;;     :config
;;     ;; (spacemacs|diminish copilot-mode "Ⓒ" "C")
;;     (spacemacs|diminish copilot-mode "🄲" "C")
;;     )
;;   )
