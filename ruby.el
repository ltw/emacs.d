;; ruby.el -- Ruby-mode packages and configurations

(defvar ruby-packages
  '(ruby-mode
    rspec-mode))
(dolist (p ruby-packages) (package-require p))

(require 'ruby-mode)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

(setenv "PATH"
        (concat (getenv "HOME") "/.rbenv/shims:"
                (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))

(setq exec-path
      (cons (concat (getenv "HOME") "/.rbenv/shims")
            (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (rspec-mode)
            (add-hook 'write-contents-hooks 'cleanup-buffer)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (setq-default tab-width 2)
            (whitespace-mode)
            (show-paren-mode)))
