;; clojure.el -- Clojure-mode packages and configurations

(defvar clojure-packages
  '(clojure-mode
    clojure-test-mode
    cider
    align-cljlet
    rainbow-delimiters
    paredit))
(dolist (p clojure-packages) (package-require p))

(require 'clojure-test-mode)
(require 'paredit)
(require 'align-cljlet)
(require 'rainbow-delimiters)  ; pretty parens


(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-brackets "blue" "Clojure brackets")
(defclojureface clojure-keyword "#bfebbf" "Clojure keywords")

;; some of these faces aren't defined. we need to debug.
;; (defun tweak-clojure-syntax ()
;;   (dolist (x '((("#?['`]*(\\|)" . 'clojure-parens))
;;                (("#?\\^?{\\|}" . 'clojure-brackets))
;;                (("\\[\\|\\]" . 'clojure-braces))
;;                ((":\\w+#?" . 'clojure-keyword))
;;                (("#?\"" 0 'clojure-double-quote prepend))
;;                (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
;;                (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
;;                ))
;;     (font-lock-add-keywords nil x)))


(add-hook 'clojure-mode-hook
          (lambda ()
;            (tweak-clojure-syntax)
            (add-hook 'write-contents-functions 'cleanup-buffer ) ; every time we save the file
            (setq-default tab-width 2)
            (paredit-mode)
            (whitespace-mode)
            (rainbow-delimiters-mode)
            (show-paren-mode)
            (define-key clojure-mode-map (kbd "C-\\") 'clojure-toggle-keyword-string)
            (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)))

(dolist (macro '(fresh conde run run* for-all defroutes describe it))
  (put-clojure-indent macro 'defun))

(dolist (route '(GET POST PUT DELETE HEAD ANY context))
  (put-clojure-indent route 2))

(setq cider-repl-wrap-history t)    ; FIFO
(setq cider-repl-history-size 1000) ; default 500
(setq cider-repl-history-file (expand-file-name (concat dotfiles-dir ".cider-repl-history")))

(provide 'clojure)
