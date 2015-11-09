;; -*- emacs-lisp -*-
;; ~/.emacs.d/init.el
;; see:
;; http://tuhdo.github.io/emacs-tutor.html

;; Built-in setups.
;;;;;;;;;;;;;;;;;;;

;; Garbage collection
(setq gc-cons-threshold (* 20 1024 1024))

;; starts server if not already running
(load "server")
(unless (server-running-p) (server-start))

;; Maximize.
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; No splashscreen, go directly to scratch buffer.
(setq inhibit-splash-screen t)

;; Even better, save last session state at exit
;; (note: default temp file to store desktop status is ~/.emacs.d/.emacs.desktop)
(desktop-save-mode 1)
;; maximum number of buffers to restore immediately (remaining buffers are restored “lazily”, when Emacs is idle)
(setq desktop-restore-eager 4)

;; For buffers visited but no longer open remeber point position
(setq save-place-limit 100)
(setq save-place-file "~/.emacs.d/saved-places")
(setq-default save-place t)
(setq save-place-forget-unreadable-files nil)
(require 'saveplace)

;; No toolbar.
(tool-bar-mode -1)

;; Ubiquitous font, reasonable size.
(add-to-list 'default-frame-alist
			 '(font . "DejaVu Sans Mono-10"))

;; Flash screen instead of beeping.
(setq visible-bell 1)

;; Show line and column number in the mode line.
(setq line-number-mode t)
(setq column-number-mode t)

;; Highlight cursor line.
(global-hl-line-mode t)

;; Stop forcing me to spell out "yes".
(fset 'yes-or-no-p 'y-or-n-p)

;; Undo level.
(setq undo-limit 100)

;; Wrap (do not truncate) long lines when using split window.
(setq truncate-partial-width-windows nil)

;; Syntax highlighting.
(setq global-font-lock-mode t)

;; Autocomplete paired brackets.
(electric-pair-mode 1)

;; See matching pairs of parentheses and other characters.
;; When point is on one of the paired characters, the other is highlighted.
(show-paren-mode 1)
;; Highlight entire bracket expression.
(setq show-paren-style 'expression)

;; Unicode / UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Ensure files have no trailing whitespace, including blank lines at EOF, when saved.
(setq-default show-trailing-whitespace t)

;; Never used this in years
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;; Indentation:
(setq c-default-style "linux")
;; Set appearance of a tab to 4 spaces.
(setq-default tab-width 4)

;; Highlights suspicious C and C++ constructions.
;; (global-cwarn-mode 1) ;; no so useful

;; Color theme: most don't get along well with global-hl-line-mode
(load-theme 'leuven t) ;; light
;;(load-theme 'tango t) ;; light
;;(load-theme 'tsdh-dark t) ;; dark but OK

;; Mouse
(global-set-key [mouse-3] 'imenu) ;; FIXME: OK with C, not C++ (does not jump to method)

;; semantic
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

;; keybinding - keep this in one place
(global-set-key (kbd "RET") 'newline-and-indent) ;; Automatically indent when press RET.
(global-set-key "\C-z"      'goto-line)          ;; C-z = suspend emacs, useless, remap
(global-set-key "\C-x\C-b"  'buffer-menu)        ;; Buffer list that does not jump in other window
(global-set-key "\C-cd"     'kill-whole-line)    ;; "a la vi dd", kill-whole-line

;; Not built-in
;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu"		.	"http://elpa.gnu.org/packages/")
						 ("melpa"	.	"http://melpa.org/packages/")))
(package-initialize)

;; add subdirs recursively
(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

;; http://github.com/jwiegley/use-package
;; - TODO 20151109 tidy configuratiion
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
;; install packages automatically if not already present on thne system
(setq use-package-always-ensure t)

;; A minor mode that guesses the indentation offset originally used for creating source code files
;; and transparently adjusts the corresponding settings in Emacs, making it more convenient to edit foreign files.
(use-package dtrt-indent)
(dtrt-indent-mode 1)

;; speedbar
(use-package sr-speedbar)
(setq sr-speedbar-auto-refresh t)
(global-set-key (kbd "<f6>") 'sr-speedbar-toggle)

;; completion
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; Turn on auto complete.
(require 'auto-complete-config)
;;(use-package auto-complete-config) ;; 20151109: error: Package `auto-complete-config-' is unavailable
(ac-config-default)

;; column line (not a package, seperate .el file)
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)
