;;(load-theme 'solarized-light t)
(load-file "~/.emacs.d/minibuf-electric-gnuemacs.el")

(global-linum-mode 1)
(global-hl-line-mode 1)

(global-visual-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(let* ((my-lisp-dir "~/.emacs.d/elpa/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(defun connect-webdev2 ()
  (interactive)
  (dired "/timand@webdev2.med.upenn.edu:~"))

(add-hook 'after-init-hook 'global-company-mode)

;; ruby things
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;;(push 'company-robe company-backends)

;; hopefully a better ido
;;(require 'flx-ido)
;;(ido-mode 1)
;;(ido-everywhere 1)
;;(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;;(setq ido-enable-flex-matching t)
;;(setq ido-use-faces nil)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.phl\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

;;(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)


(when (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil :font "Source Code Pro-12"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(dired-use-ls-dired nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indent-tabs-mode nil)
 '(magit-use-overlays nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(php-file-patterns
   (quote
    ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.phl\\'")))
 '(save-place t nil (saveplace))
 '(savehist-mode t)
 '(show-paren-mode t)
 '(sr-speedbar-delete-windows t)
 '(sr-speedbar-right-side nil)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.saves")
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-symbol-face ((t (:foreground "gold4")))))
