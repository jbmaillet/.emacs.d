;; -*- emacs-lisp -*-
;; ~/.emacs.d/init.el
;; see:
;; http://tuhdo.github.io/emacs-tutor.html

;; Built-in setups.
;;;;;;;;;;;;;;;;;;;

;; Garbage collection.
(setq gc-cons-threshold (* 20 1024 1024))

;; Starts server if not already running.
(load "server")
(unless (server-running-p) (server-start))

;; Maximize.
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; No toolbar.
(tool-bar-mode -1)

;; No splashscreen.
(setq inhibit-splash-screen t)

;; Flash screen instead of beeping.
(setq visible-bell 1)

;; Save last session state at exit.
;; (note: default temp file to store desktop status is ~/.emacs.d/.emacs.desktop)
(desktop-save-mode 1)
;; maximum number of buffers to restore immediately (remaining buffers are restored “lazily”, when Emacs is idle)
(setq desktop-restore-eager 4)

;; Add recent files to the 'File' menu.
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; For buffers visited but no longer open, remember point position.
(setq save-place-limit 100)
(setq save-place-file "~/.emacs.d/saved-places")
(setq-default save-place t)
(setq save-place-forget-unreadable-files nil)
(require 'saveplace)

;; Ignore case when using completion for file names.
(setq read-file-name-completion-ignore-case t)

;; Navigate among windows by shift-up, shift-down, shift-left, shift-right.
(windmove-default-keybindings)

;; Ubiquitous font, reasonable size.
(add-to-list 'default-frame-alist
			 '(font . "DejaVu Sans Mono-10"))

;; Show line and column number in the mode line.
(setq line-number-mode t)
(setq column-number-mode t)

;; Highlight cursor line.
(global-hl-line-mode t)

;; Sentences end with one space.
(setq sentence-end-double-space nil)

;; Stop forcing me to spell out "yes".
(fset 'yes-or-no-p 'y-or-n-p)

;; Undo level.
(setq undo-limit 160000) ; default value 80000

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
;; (setq show-paren-style 'expression) ;; not so useful

;; Unicode / UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Visualize trailing whitespace...
(setq show-trailing-whitespace t)
;; ...but disable it in certain modes where it doesn't make sense.
(add-hook 'buffer-menu-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'shell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; Require a newline at the end of files.
(setq require-final-newline t)

;; Never used this in years
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;; Indentation:
(setq c-default-style "linux")
;; Set appearance of a tab to 8 spaces.
(setq-default c-basic-offset 8
			  tab-width 8
			  indent-tabs-mode t)

;; Highlights suspicious C and C++ constructions.
;; (global-cwarn-mode 1) ;; not so useful

;; Color theme: most don't get along well with global-hl-line-mode
;;(load-theme 'leuven t) ;; light
(load-theme 'tango t) ;; light
;;(load-theme 'tsdh-dark t) ;; dark but OK

;; Mouse
(global-set-key [mouse-3] 'imenu) ;; FIXME: OK with C, not C++ (does not jump to method)

;; semantic
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)

;; shell: do not open a new window with the list of completions, use company instead.
(add-hook 'shell-mode-hook #'company-mode)
(eval-after-load 'shell-mode
  '(define-key shell-mode-map (kbd "TAB") #'company-manual-begin))

;; term: always use bash
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Functions
;;;;;;;;;;;;

;; No prompt for "...has a running process” on killing term
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-exec-hook 'set-no-process-query-on-exit) ;; TODO 20151112 same for shell-mode

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

;; Not built-in
;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(
			 ("gnu"		.	"http://elpa.gnu.org/packages/")
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

;; TODO, FIXME etc
(use-package fic-mode)
(add-hook 'c-mode-hook 'turn-on-fic-mode)
(add-hook 'c++-mode-hook 'turn-on-fic-mode)

;; completion
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; Turn on auto complete.
;;(require 'auto-complete-config) ;; 20151111: definitely broken?
;;(use-package auto-complete-config) ;; 20151109: error: Package `auto-complete-config-' is unavailable
;:(ac-config-default)

;; Tip of the day.
;(use-package totd)
;(totd-start)

;; column line (not a package, seperate .el file)
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)

;; Keybinding - try to keep this in one place
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From the manual:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;; "Sequences consisting of C-c and a letter (either upper or lower case) are reserved for users"
;; "Function keys F5 through F9 without modifier keys are also reserved for users to define."

(global-set-key (kbd "RET") 'newline-and-indent) ;; Automatically indent when press RET.
(global-set-key "\C-z"      'goto-line)          ;; C-z = suspend emacs, useless, remap.
(global-set-key "\C-x\C-b"  'buffer-menu)        ;; Buffer list that does not jump in other window.
(global-set-key "\C-cd"     'kill-whole-line)    ;; "a la vi dd", kill-whole-line.

;; Cheat sheet - things I tend to forget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-g g    goto line #
;; M-^      join line, "a la vi J
;; On xfce, disable the C-M for this:
;; C-M-f    Move forward over a balanced expression (forward-sexp).
;; C-M-b    Move backward over a balanced expression (backward-sexp).
;; C-M-n    Move forward over a parenthetical group (forward-list).
;; C-M-p    Move backward over a parenthetical group (backward-list).
;; C-M-u    Move up in parenthesis structure (backward-up-list).
;; C-M-d    Move down in parenthesis structure (down-list).
;; Dual screen:
;; C-x 5 2  New frame
;; C-x 5 o  Swith frame
