;;; FED - Fun Emacs Development! YAY

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

(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      cider
                      align-cljlet
                      color-theme
                      jujube-theme
                      markdown-mode
                      paredit))
(dolist (p my-packages) (package-require p))

(require 'clojure-test-mode)
(require 'paredit)
(require 'align-cljlet)
(load-theme 'jujube t)

;; general configuration options
(setq require-final-newline "ask")        ; insure all files end in \n
(setq print-escape-newlines t)            ; newlines in strings get print as \n
(setq find-file-visit-truename t)         ; load file symbolic link points at
(setq completion-auto-help t)             ; I want as much help as I can get
(setq max-lisp-eval-depth 1500)           ; seems to let me have more open files
(setq visible-bell t)                     ; noise is evil
(setq kill-whole-line t)                  ; include new-line with ctrl-k
(setq case-fold-search t)                 ; make searches case insensitive
(setq make-backup-files nil)              ; "No. More. Tilde. Files!" -- Faye Dunaway
(setq sentence-end-double-space nil)      ; don't put two spaces after a dang sentence
(transient-mark-mode t)                   ; show marked region
(set-face-background 'region "LightCyan") ; color of marked region

;; disable scroll bars, tool bars and menu bars.
(dolist (mode '(scroll-bar-mode tool-bar-mode menu-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; clipboard
(setq x-select-enable-clipboard t)		; kill-ring merged with clipboard
(setq kill-ring-max 200)                        ; a bigger kill-ring (default = 30)
(if (eq system-type 'darwin)                    ; integrate with mac clipboard
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

;; Always newline-and-indent
(global-set-key (kbd "RET")         'newline-and-indent)

;; enable line / column numbers
(column-number-mode t)                                   ; show column numbers in mode bar
(global-linum-mode t)                                    ; show line numbers on left hand margin
(setq linum-format "%3d ")                               ; pad line numbers with a right-hand space. also enforce 3-char line-number
(setq-default indent-tabs-mode nil)                      ; never insert tab literals
(setq whitespace-style '(face trailing tabs))            ; select certain types of whitespace for highlighting

;; enable ido everywhere with fuzzy-matching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; better filename distinguishing
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; enable smooth scrolling with a 3-line margin
(setq scroll-step            1      ; how many lines to scroll at a time
      scroll-margin          3      ; start scrolling 3 lines from edge
      scroll-conservatively  10000) ; move judiciously

;; clojure mode-specific stuff
;; pasted from https://github.com/mpenet/emax/blob/master/config/modes.el#L89
(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-brackets "blue" "Clojure brackets")
(defclojureface clojure-keyword "#bfebbf" "Clojure keywords")

(defun tweak-clojure-syntax ()
  (dolist (x '((("#?['`]*(\\|)" . 'clojure-parens))
               (("#?\\^?{\\|}" . 'clojure-brackets))
               (("\\[\\|\\]" . 'clojure-braces))
               ((":\\w+#?" . 'clojure-keyword))
               (("#?\"" 0 'clojure-double-quote prepend))
               (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
               (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
               ))
    (font-lock-add-keywords nil x)))

(add-hook 'clojure-mode-hook            ; when in clojure-mode
          (lambda ()
            (dolist (mode '(paredit-mode
                            whitespace-mode
                            show-paren-mode))
              (funcall mode +1))
            (tweak-clojure-syntax)
            (setq-default tab-width 2)
            (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)))

(dolist (macro '(fresh conde run run* for-all))
  (put-clojure-indent macro 'defun))
