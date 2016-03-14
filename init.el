;;; init.el --- user init file      -*- no-byte-compile: t -*-
;;; Commentary:
;;this is my .emacs file, for osx at work

;;; Code:

;; c+shift+E in intellij should open with emacsclient.
(server-start)

(setq ring-bell-function 'ignore)
(setq load-prefer-newer t)

(let* ((my-lisp-dir "~/.emacs.d/elpa/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))m

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(require 'editorconfig)

;; Helm
(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; from https://www.reddit.com/r/emacs/comments/30yer0/helm_and_recentf_tips/
(define-key helm-map (kbd "C-x b")  'helm-mini)
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found))

(helm-mode 1)

;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;;(load-theme 'solarized-light t)
(load-theme 'base16-tomorrow-dark t)
(load-file "~/.emacs.d/minibuf-electric-gnuemacs.el")

;; tab bar?
;;(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;;(require 'tabbar-ruler)


;; to show rails log files better
(require 'tty-format)

(setq magit-last-seen-setup-instructions "1.4.0")

;(sml/setup)
;(sml/apply-theme 'automatic)

;; org mode stuff
;; C-C n to make a Note
;;(setq org-default-notes-file (concat org-directory "/captured_notes.org"))
(define-key global-map "\C-cn" 'org-capture)

;(require 'smooth-scrolling)
(global-linum-mode 1)
(global-hl-line-mode 1)

(global-visual-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(defun connect-webdev2 ()
  "Use tramp to get to webdev2."
  (interactive)
  (dired "/timand@webdev2.med.upenn.edu:~"))

(add-hook 'after-init-hook 'global-company-mode)

;; flycheck mode was annoying when ruby-lint was there. Not now though
;;(add-hook 'after-init-hook #'global-flycheck-mode)


;; ruby things
;;(require 'flymake-ruby)
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;;(push 'company-robe company-backends)

;;hopefully a better ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;;disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Display ido results vertically, rather than horizontally
;; https://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () "No docs." (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys ()
  "C - n /p is more intuitive in vertical layout."
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)


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

;; projects and trees
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(setq projectile-switch-project-action 'neotree-projectile-action)
;; another option
;;(require 'projectile-speedbar)

;; (when (member "Source Code Pro" (font-family-list))
;;   (set-face-attribute 'default nil :font "Source Code Pro-12"))

;; (when (member "CamingoCode" (font-family-list))
;;   (set-face-attribute 'default nil :font "CamingoCode-12"))

;; (when (member "PragmataPro" (font-family-list))
;;   (set-face-attribute 'default nil :font "PragmataPro-12"))


;; javascript stuff
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#002451"))
 '(ansi-term-color-vector
   [unspecified "#000000" "#e92f2f" "#0ed839" "#dddd13" "#3b48e3" "#f996e2" "#3b48e3" "#ababab"] t)
 '(apropos-do-all t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes nil)
 '(custom-safe-themes t)
 '(desktop-save-mode t)
 '(dired-use-ls-dired nil)
 '(editorconfig-exec-path "/usr/local/bin/editorconfig")
 '(fci-rule-color "#003f8e")
 '(flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
 '(flycheck-ruby-rubylint-executable nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-flycheck-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(indent-tabs-mode nil)
 '(js-indent-level 4)
 '(line-spacing 0.3)
 '(magit-diff-options (quote ("--ignore-all-space")))
 '(magit-diff-use-overlays nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-use-overlays nil)
 '(neo-click-changes-root nil)
 '(neo-cwd-line-style (quote button))
 '(neo-theme (quote nerd))
 '(org-agenda-files (quote ("~/orgmode/worklog.org")))
 '(org-capture-templates
   (quote
    (("n" "item" item
      (file "~/orgmode/captured_notes.org")
      ""))))
 '(org-mobile-directory "~/Dropbox/orgmode")
 '(org-mobile-inbox-for-pull "~/orgmode/from-mobile.org")
 '(package-selected-packages
   (quote
    (js2-mode paradox helm-dash rubocop ppd-sr-speedbar project-persist project-persist-drawer yaml-mode web-mode vagrant-tramp vagrant tabbar-ruler steady-theme sr-speedbar solarized-theme smooth-scrolling smart-tabs-mode smart-mode-line rspec-mode robe reveal-in-osx-finder rbenv rails-log-mode projectile-rails project-explorer powerline php-auto-yasnippets org-plus-contrib omniref neotree markdown-mode magit-filenotify lenlen-theme launchctl iplayer highlight-current-line helm-themes helm-projectile helm-orgcard helm-ag guide-key go-mode gitlab gitignore-mode gitconfig-mode git-gutter+ ggtags flymake-ruby flycheck flx-ido ember-mode editorconfig company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode base16-theme auto-compile ag abc-mode)))
 '(paradox-github-token t)
 '(php-file-patterns
   (quote
    ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.phl\\'")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-completion-system (quote helm))
 '(projectile-enable-caching t)
 '(projectile-use-git-grep t)
 '(save-place t nil (saveplace))
 '(savehist-mode t)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sr-speedbar-delete-windows t)
 '(sr-speedbar-right-side nil)
 '(standard-indent 2)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.saves")
 '(truncate-lines t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002451" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "PragmataPro"))))
 '(neo-banner-face ((t (:foreground "#93a1a1"))))
 '(neo-button-face ((t (:underline nil))))
 '(neo-dir-link-face ((t (:foreground "#268bd2" :height 1.0))))
 '(neo-expand-btn-face ((t (:foreground "#93a1a1" :height 1.0))))
 '(neo-file-link-face ((t (:foreground "#657b83"))))
 '(neo-header-face ((t (:foreground "#268bd2"))))
 '(neo-root-dir-face ((t (:foreground "#586e75" :weight bold))))
 '(web-mode-symbol-face ((t (:foreground "gold4")))))
