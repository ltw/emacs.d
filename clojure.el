;; clojure.el -- Clojure-mode packages and configurations

(defvar clojure-packages
  '(clojure-mode
    clojure-test-mode
    cider
    align-cljlet
    paredit))
(dolist (p clojure-packages) (package-require p))

(require 'clojure-test-mode)
(require 'paredit)
(require 'align-cljlet)

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

(provide 'clojure)
