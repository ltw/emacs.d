;;; FED - Fun Emacs Development! YAY

;; add .emacs.d to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

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
    markdown-mode))
(dolist (p my-packages) (package-require p))

(require 'clojure)         ; load clojure-specific configurations

(load-theme 'jujube t)

;; general configuration options
(setq require-final-newline "ask"         ; insure all files end in \n
      print-escape-newlines t             ; newlines in strings get print as \n
      find-file-visit-truename t          ; load file symbolic link points at
      completion-auto-help t              ; I want as much help as I can get
      max-lisp-eval-depth 1500            ; seems to let me have more open files
      visible-bell t                      ; noise is evil
      kill-whole-line t                   ; include new-line with ctrl-k
      case-fold-search t                  ; make searches case insensitive
      make-backup-files nil               ; "No. More. Tilde. Files!" -- Faye Dunaway
      sentence-end-double-space nil)      ; don't put two spaces after a dang sentence
(transient-mark-mode t)                   ; show marked region
(set-face-background 'region "LightCyan") ; color of marked region
(defalias 'yes-or-no-p 'y-or-n-p)

;; thee tabs, thee
(setq tab-width 2
      indent-tabs-mode nil)

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
(global-set-key (kbd "RET")         'newline-and-indent)          ; always newline-and-indent
(global-set-key (kbd "C-;")         'comment-or-uncomment-region) ; comments are nice

;; enable line / column numbers
(column-number-mode t)                                   ; show column numbers in mode bar
(global-linum-mode t)                                    ; show line numbers on left hand margin
(setq linum-format "%3d ")                               ; pad line numbers with a right-hand space. also enforce 3-char line-number
(setq-default indent-tabs-mode nil)                      ; never insert tab literals
(setq whitespace-style '(face trailing tabs))            ; select certain types of whitespace for highlighting

;; enable ido everywhere with fuzzy-matching
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-everywhere t)
(ido-mode 1)

;; better filename distinguishing
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; enable smooth scrolling with a 3-line margin
(setq scroll-step            1      ; how many lines to scroll at a time
      scroll-margin          3      ; start scrolling 3 lines from edge
      scroll-conservatively  10000) ; move judiciously
