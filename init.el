;;; FED - Fun Emacs Development! YAY

;; add .emacs.d to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; always use utf-8. we write software for the whole world.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; package.el configuration
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(defvar my-packages
  '(color-theme
    jujube-theme
    auto-complete
    powerline
    smex
    markdown-mode))

(dolist (p my-packages) (package-require p))


(require 'powerline)
(powerline-default-theme)

(load-theme 'jujube t)

;; general configuration options
(setq require-final-newline "visit-save"     ; ensure all files end in \n
      print-escape-newlines t                ; newlines in strings get print as \n (well, it's supposed to work that way)
      find-file-visit-truename t             ; load file symbolic link points at
      completion-auto-help t                 ; I want as much help as I can get
      max-lisp-eval-depth 1500               ; seems to let me have more open files
      visible-bell t                         ; noise is evil
      kill-whole-line t                      ; include new-line with ctrl-k
      case-fold-search t                     ; make searches case insensitive
      make-backup-files nil                  ; "No. More. Tilde. Files!" -- Faye Dunaway
      save-interprogram-paste-before-kill t  ; guarantees the thing you're overwriting ends up on kill ring
      sentence-end-double-space nil)         ; don't put two spaces after a dang sentence
(delete-selection-mode t)                    ; overwrite a current marked selection when typing
(transient-mark-mode t)                      ; show marked region
(set-face-background 'region "LightCyan")    ; color of marked region
(defalias 'yes-or-no-p 'y-or-n-p)

; don't bug me before saving
(add-hook 'find-file-hook (lambda () (setq buffer-save-without-query t)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

;; thee tabs, thee
(setq tab-width 2
      indent-tabs-mode nil)

;; save history
;;  we save kill ring and search rings too
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(setq savehist-file (expand-file-name (concat dotfiles-dir ".savehist")))
(setq history-length 1000)
(require 'savehist)
(savehist-mode t)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; disable scroll bars, tool bars and menu bars.
(dolist (mode '(scroll-bar-mode tool-bar-mode menu-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; clipboard
(setq x-select-enable-clipboard t ; kill-ring merged with clipboard
      kill-ring-max 200)          ; a bigger kill-ring (default = 30)
(if (eq system-type 'darwin)       ; integrate with mac clipboard
    (progn
      (defun mac-copy ()
        (shell-command-to-string "pbpaste"))

      (defun mac-paste (text &optional push)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))

      (setq interprogram-cut-function 'mac-paste)
      (setq interprogram-paste-function 'mac-copy)
      ))

;; navigate between frames
(require 'windmove)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)


;; various key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)                          ; nicer buffer listing
(global-set-key (kbd "M-/") 'hippie-expand)                        ; much nicer expansion
(global-set-key (kbd "RET")         'newline-and-indent)           ; always newline-and-indent
(global-set-key (kbd "C-;")         'comment-or-uncomment-region)  ; comments are nice
(global-set-key (kbd "C-x C-j") 'goto-line)

;; enable line / column numbers
(column-number-mode t)                                   ; show column numbers in mode bar
(global-linum-mode t)                                    ; show line numbers on left hand margin
(setq linum-format "%3d ")                               ; pad line numbers with a right-hand space. also enforce 3-char line-number
(setq-default indent-tabs-mode nil)                      ; never insert tab literals
(setq whitespace-style '(face trailing tabs))            ; select certain types of whitespace for highlighting

;; kill dat splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; enable ido everywhere with fuzzy-matching
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-everywhere t)
(ido-mode 1)

;; better filename distinguishing
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; keep track of where I left off in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory ".places"))

;; enable smooth scrolling with a 3-line margin
(setq scroll-step            1      ; how many lines to scroll at a time
      scroll-margin          3      ; start scrolling 3 lines from edge
      scroll-conservatively  10000) ; move judiciously

;; make M-x so much smarter with smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; cleanup stuff.
;; stolen from abedra, who stole it from technomancy.
;; we're all just thieves, swiping in the dark.
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(require 'clojure)         ; load clojure-specific configurations

;; use markdown-mode for markdown files
(autoload 'markdown-mode "markdown-mode"
     "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; buffer navigation
;; ----------------------------------------------------------------------
;;     Original yic-buffer.el
;;     From: choo@cs.yale.edu (young-il choo)
;;     Date: 7 Aug 90 23:39:19 GMT
;;
;;     Little more sophisticated for my private use:
;;     Aug 7 1995 "Jari Aalto" <jaalto@tre.tele.nokia.fi> Public domain.
;;
;;     Added nrepl to yic-ignore-re
;;     Feb 19 2014 "Jason Yanowitz" <f@ug.ly>
;; ----------------------------------------------------------------------

(defconst yic-ignore-re
  (concat
   "^ "                 ;hidden buffers
   "\\|completion\\|summary"
   "\\|buffer list\\|help$\\|ispell\\|abbrev"
   "\\|nrepl"
   "\\|temp\\|tmp\\|post\\|tff"
   )
  "*Buffers to ignore when changing to another.")

(defun yic-next (list)
  "Switch to next buffer in list, skipping unwanted ones."
  (let* ((re  yic-ignore-re)
         buffer go
         )
    (while (and list (null go))
      (setq buffer (car list))
      (if (string-match re (buffer-name buffer))                ;skip over
          (setq list (cdr list))
        (setq go buffer)))
    (if go   (switch-to-buffer go))
    ))

(defun yic-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (yic-next (reverse (buffer-list))))

(defun yic-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))

(global-set-key "\M-n" 'yic-prev-buffer)
(global-set-key "\M-m" 'yic-next-buffer)

(defun yic-config-keys ()
  "config keys for various modes"
  (local-set-key "\M-n" 'yic-prev-buffer)
  (local-set-key "\M-m" 'yic-next-buffer))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:inherit mode-line :background "color-125")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

(setq debug-on-error t)
