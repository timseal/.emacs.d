;;; init.el --- user init file      -*- no-byte-compile: t -*-
;;; Commentary:
;;this is my .emacs file, for osx at work

;;; Code:

;; c+shift+E in intellij should open with emacsclient.
;;(server-start)


(let* ((my-lisp-dir "~/.emacs.d/elpa/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))


;;;;;;;;;;;;;;;;;;;;;; GENERAL UI AND BASIC EDITING STUFF ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;  ie basic emacs config only, no packages ;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/minibuf-electric-gnuemacs.el")

                                        ;(require 'smooth-scrolling)
                                        ;(global-linum-mode 1)


(global-hl-line-mode nil)

(global-visual-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(setq ring-bell-function 'ignore)
(setq load-prefer-newer t)



(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;;;;;;;;;;;;;;;;;;;;;; END OF GENERAL UI AND BASIC EDITING STUFF ;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;  PACKAGES SETUP    ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                        ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages are now set up!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes n stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(load-theme 'solarized-light t)
;;(load-theme 'base16-tomorrow-dark t)
;;(load-theme 'labburn t)
(setq custom-safe-themes t)
;;(load-theme 'spacemacs-dark)
;;(load-theme 'pastelmac)

;;; solaire mode makes file-visiting windows a bit brighter
(solaire-global-mode +1)
;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;; NOTE: This is (allegedly) necessary for themes in the doom-themes package!
(solaire-mode-swap-bg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doom themes, https://github.com/hlissner/emacs-doom-themes
(require 'doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-nord-light t)
;; Enable custom neotree theme (all-the-icons must be installed!)
;;(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
(doom-themes-treemacs-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)
;;(let ((height (face-attribute 'default :height)))
;;  (set-face-attribute 'linum nil :height height))
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(display-time-mode)
;; (require 'smart-mode-line)
;; (setq powerline-arrow-shape 'curve)
;; (setq powerline-default-separator-dir '(right . left))
;; (setq sml/theme 'powerline)
;; (setq sml/mode-width 0)
;; (setq sml/name-width 20)
;; (rich-minority-mode 1)
;; (setf rm-blacklist "")
;; (sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various package load and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'left)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
     "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
     (list
      (cond
	((or (string-equal "*" (substring (buffer-name) 0 1))
	     (memq major-mode '(magit-process-mode
				magit-status-mode
				magit-diff-mode
				magit-log-mode
				magit-file-mode
				magit-blob-mode
				magit-blame-mode
				)))
	 "Emacs")
	((derived-mode-p 'prog-mode)
	 "Editing")
	((derived-mode-p 'dired-mode)
	 "Dired")
	((memq major-mode '(helpful-mode
			    help-mode))
	 "Help")
	((memq major-mode '(org-mode
			    org-agenda-clockreport-mode
			    org-src-mode
			    org-agenda-mode
			    org-beamer-mode
			    org-indent-mode
			    org-bullets-mode
			    org-cdlatex-mode
			    org-agenda-log-mode
			    diary-mode))
	 "OrgMode")
	(t
	 (centaur-tabs-get-group-name (current-buffer))))))
   :hook
   (dashboard-mode . centaur-tabs-local-mode)
   (term-mode . centaur-tabs-local-mode)
   (calendar-mode . centaur-tabs-local-mode)
   (org-agenda-mode . centaur-tabs-local-mode)
   (helpful-mode . centaur-tabs-local-mode)
   :bind
   ("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t s" . centaur-tabs-counsel-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups)
  )

;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

(use-package which-key)
(which-key-mode)

;; RSS with elfeed
(require 'elfeed)
(require 'elfeed-goodies)
(elfeed-goodies/setup)
;;(global-set-key (kbd "C-x r") 'elfeed)

;;;;;;;;;;

;; conflicts with org mode
;; (use-package yasnippet)
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'auto-compile)
;; (auto-compile-on-load-mode 1)
;; (auto-compile-on-save-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'helm)
;; (require 'helm-config)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; ;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "C-x f") 'helm-find)
;; (global-set-key (kbd "C-x C-f") 'find-file)
;; (helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab bar?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; source is under elpa directory, but I git cloned it myself.
;; (setq tabbar-ruler-global-tabbar t)    ; get tabbar
;; (setq tabbar-ruler-global-ruler nil)     ; get global ruler
;; (setq tabbar-ruler-popup-menu t)       ; get popup menu.
;; (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;; (setq tabbar-ruler-popup-scrollbar nil)  ; show scroll-bar on mouse-move
;; (require 'tabbar-ruler)


;; to show rails log files better
;;(require 'tty-format)

(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)



;; org mode stuff
;;(use-package org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; C-C n to make a Note
;;(setq org-default-notes-file (concat org-directory "/captured_notes.org"))
;;(define-key global-map "\C-cn" 'org-capture)

;; This is from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))



;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to see webdev4 at PMACS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun connect-webdev4 ()
  "Use tramp to get to webdev4."
  (interactive)
  (dired "/timand@webdev4.med.upenn.edu:~"))

;;;; we are on a mac:
                                        ;(when (memq window-system '(mac ns))
                                        ;  (exec-path-from-shell-initialize))



(add-hook 'after-init-hook 'global-company-mode)

;; flycheck mode was annoying when ruby-lint was there. Not now though
;;(add-hook 'after-init-hook #'global-flycheck-mode)


;; ruby things
;;(add-hook 'ruby-mode-hook 'ruby-electric-mode)
;;(require 'flymake-ruby)
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;;(push 'company-robe company-backends)

;;hopefully a better ido
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; ;;disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode)
;; ;; Default for ido is C-s amd C-r for prev/next. Vertically this makes no sense, so we want C-n and C-p
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)


;;(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.phl\\'" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

;; projects and trees

;; (use-package projectile-rails)
;; (projectile-rails-global-mode)

;; (setq projectile-enable-caching t)


;; ;; asks for file to open when project is switched
;; (setq projectile-switch-project-action 'helm-projectile-find-file)

;; ;; turns on helm bindings for projectile
;; (helm-projectile-on)


;; (require 'rspec-mode)

;;(projectile-global-mode)
;;(add-hook 'projectile-mode-hook 'projectile-rails-on)
;;(setq projectile-completion-system 'helm)
;;(helm-projectile-on)

;;(setq projectile-switch-project-action 'neotree-projectile-action)
;;(setq projectile-switch-project-action 'helm-projectile)

;; another option
;;(require 'projectile-speedbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts (use one of the below)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when (member "Source Code Pro" (font-family-list))
;;   (set-face-attribute 'default nil :font "Source Code Pro-12"))

;; (when (member "CamingoCode" (font-family-list))
;;   (set-face-attribute 'default nil :font "CamingoCode-12"))

(when (member "PragmataPro" (font-family-list))
  (set-face-attribute 'default nil :font "PragmataPro-12"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   THE END. CUSTOMIZE WRITES THE REST ON ITS OWN    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(ansi-term-color-vector
   [unspecified "#061229" "#d07346" "#99bf52" "#fbd461" "#5299bf" "#9989cc" "#5299bf" "#b8bbc2"] t)
 '(apropos-do-all t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(doom-acario-light))
 '(custom-safe-themes t)
 '(desktop-save-mode nil)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-use-ls-dired nil)
 '(diredfl-global-mode t nil (diredfl))
 '(display-time-mode t)
 '(editorconfig-exec-path "/usr/local/bin/editorconfig")
 '(elfeed-curl-program-name "/usr/local/opt/curl-openssl/bin/curl")
 '(elfeed-feeds
   '("https://www.nerdfitness.com/home/feed/" "https://www.cordcuttersnews.com/feed/" "https://blog.burrow.com/rss/" "http://feeds.feedburner.com/TheWirecutter" "https://kinjadeals.theinventory.com/rss" "http://clashroyalearena.com/feed" "https://www.brotherlygame.com/rss/index.xml" "https://www.thoughtworks.com/rss/insights.xml" "https://martinfowler.com/feed.atom" "http://www.sandimetz.com/blog?format=RSS" "http://blog.cleancoder.com/atom.xml" "http://pdabrowski.com/blog/feed/" "https://gorails.com/blog.rss" "https://evilmartians.com/chronicles.atom" "http://www.yegor256.com/rss.xml" "https://medium.com/feed/rubyinside" "https://blog.eq8.eu/feed.xml" "https://www.speedshop.co/feed.xml" "http://rubyland.news/feed.rss" "https://iridakos.com/feed.xml" "http://www.rubyflow.com/rss" "http://feeds.feedburner.com/arkency.xml" "http://feeds.feedburner.com/AndroidPolice" "http://www.xda-developers.com/feed/" "http://www.snookerbacker.com/feed/" "https://freecoursesite.com/feed/" "https://courseclub.net/feed/" "https://couchtripper.com/blog/?feed=rss2" "https://fbrls.blogspot.com/feeds/posts/default" "https://tutsgalaxy.com/feed/" "http://docuwiki.net/index.php?title=Special:Newpages&feed=rss" "https://www.thatjeffsmith.com/feed/" "https://vscodecandothat.com/feed.xml" "https://johnbeatty.co/feed/" "http://livebootlegconcert.blogspot.com/feeds/posts/default" "http://musicforprogramming.net/rss.php" "https://tim.blog/feed/" "http://feeds.feedburner.com/TheBulletproofMusician" "http://feeds.feedburner.com/StyleGirlfriend" "http://www.azbilliards.com/rss/feed.xml" "https://about.gitlab.com/atom.xml" "http://xkcd.com/rss.xml" "http://www.smbc-comics.com/rss.php" "http://syndication.thedailywtf.com/TheDailyWtf" "https://www.theguardian.com/info/series/digital-blog/rss" "https://linuxize.com/index.xml" "https://vscode.rocks/rss.xml" "https://ducfilan.wordpress.com/feed/" "https://www.enpass.io/blog/feeds/rss/" "http://feeds.feedburner.com/ScottHanselman" "http://unfuckyourhabitat.tumblr.com/rss" "http://unclutterer.com/feed/"))
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(fci-rule-color "#383a42")
 '(flycheck-rubocop-lint-only t)
 '(flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-flycheck-mode nil)
 '(global-nlinum-mode t)
 '(gnus-logo-colors '("#2fdbde" "#c0c0c0") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(indent-tabs-mode nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(js-indent-level 4)
 '(line-spacing 0.3)
 '(mac-auto-operator-composition-mode t)
 '(mac-mouse-wheel-smooth-scroll t)
 '(magit-diff-options '("--ignore-all-space"))
 '(magit-diff-use-overlays nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-use-overlays nil)
 '(neo-click-changes-root nil)
 '(neo-cwd-line-style 'button)
 '(neo-theme 'nerd)
 '(nlinum-highlight-current-line t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(ns-alternate-modifier '(:ordinary meta :function alt :mouse alt))
 '(objed-cursor-color "#e45649")
 '(org-agenda-files '("~/orgmode/worklog.org"))
 '(org-capture-templates
   '(("n" "item" item
      (file "~/orgmode/captured_notes.org")
      "")) t)
 '(org-mobile-directory "~/Dropbox/orgmode")
 '(org-mobile-inbox-for-pull "~/orgmode/from-mobile.org")
 '(package-selected-packages
   '(solaire-mode hl-block-mode diredfl treemacs-icons-dired treemacs-magit elfeed elfeed-goodies php-mode taskpaper-mode nlinum all-the-icons doom-modeline doom-themes moody centaur-tabs org gitlab-ci-mode gitlab-ci-mode-flycheck sqlformat ssh-config-mode markdown-preview-mode magit treemacs-projectile treemacs dockerfile-mode vue-html-mode enh-ruby-mode ruby-electric leuven-theme paper-theme pastelmac-theme yafolding which-key use-package el-get alect-themes sql-indent format-sql sqlup-mode e2ansi multi-term anzu slack spaceline moe-theme smart-mode-line-powerline-theme exec-path-from-shell helm-flycheck osx-plist ruby-refactor spotlight farmhouse-theme majapahit-theme dakrone-theme hydandata-light-theme spacemacs-theme labburn-theme helm-projectile helm-ag helm-descbinds helm-ls-git helm-smex smex async company-statistics dash helm-company hydra rich-minority bundler company-flx company-shell ido-vertical-mode js2-mode paradox rubocop ppd-sr-speedbar project-persist project-persist-drawer yaml-mode web-mode vagrant-tramp vagrant steady-theme sr-speedbar solarized-theme smooth-scrolling smart-tabs-mode smart-mode-line rspec-mode robe reveal-in-osx-finder rbenv rails-log-mode projectile-rails project-explorer php-auto-yasnippets org-plus-contrib omniref neotree markdown-mode magit-filenotify lenlen-theme launchctl iplayer highlight-current-line guide-key go-mode gitlab gitignore-mode gitconfig-mode git-gutter+ ggtags flycheck flx-ido ember-mode editorconfig company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode base16-theme auto-compile ag abc-mode))
 '(paradox-github-token t)
 '(php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.phl\\'"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-completion-system 'ido)
 '(projectile-enable-caching t)
 '(projectile-use-git-grep t)
 '(rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(rspec-use-bundler-when-possible t)
 '(rspec-use-rake-when-possible t)
 '(rspec-use-spring-when-possible nil)
 '(save-place t nil (saveplace))
 '(savehist-mode t)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/mode-width (if (eq (powerline-current-separator) 'arrow) 'right 'full))
 '(sml/pos-id-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'powerline-active1 'powerline-active2)))
     (:propertize " " face powerline-active2)))
 '(sml/pos-minor-modes-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active1 'sml/global)))
     (:propertize " " face sml/global)))
 '(sml/pre-id-separator
   '(""
     (:propertize " " face sml/global)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'sml/global 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-minor-modes-separator
   '(""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active2 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes))
 '(spaceline-helm-mode t)
 '(split-height-threshold 25)
 '(sr-speedbar-delete-windows t)
 '(sr-speedbar-right-side nil)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.saves")
 '(truncate-lines t)
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
;; '(default ((t (:inherit nil :stipple nil :background "#002451" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "PragmataPro"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-banner-face ((t (:foreground "#93a1a1"))))
 '(neo-button-face ((t (:underline nil))))
 '(neo-header-face ((t (:foreground "#268bd2")))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
