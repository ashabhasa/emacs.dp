;;; init-elixir.el --- Support for the Elixir language
 ;;; Commentary:
 ;;; Code:

(require 'eglot)

;;; Code: I need this only for elixir-format
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))


;;; Code: install heex-ts-mode and elixir-ts-mode
(unless (package-installed-p 'heex-ts-mode)
  (package-install 'heex-ts-mode)
  )

(unless (package-installed-p 'elixir-ts-mode)
  (package-install 'elixir-ts-mode)
  )

;; This is optional. It automatically runs `M-x eglot` for you whenever you are in `elixir-mode`
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)

;; (add-to-list 'eglot-server-programs '(elixir-ts-mode  "~/dev/elixir/elixir-ls-gh/release_07_04_2026/language_server.sh"))

(with-eval-after-load 'eglot
  (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode)
                   eglot-server-programs
                   nil nil #'equal)
        (eglot-alternatives
         '(("~/dev/elixir/expert/apps/expert/_build/prod/rel/plain/bin/start_expert" "--stdio")))))

;; remap % as a punctuation
(add-hook 'elixir-ts-mode-hook
          (lambda ()
            (modify-syntax-entry ?% "." (syntax-table))))

(unless (package-installed-p 'exunit)
  (package-install 'exunit))

(when (maybe-require-package 'exunit)
  (add-hook 'elixir-ts-mode-hook 'exunit-mode))

;; (when (maybe-require-package 'exunit)
;; (add-hook 'elixir-mode-hook 'exunit-mode))

;; use tree sitter
(setq major-mode-remap-alist
      '((elixir-mode . elixir-ts-mode)
        (heex-mode . heex-ts-mode)))

;; run mox test from emacs
;; copied from https://dev.to/erickgnavar/minimal-setup-for-elixir-development-in-emacs-5k4
(defun my/mix-run-test (&optional at-point trace)
  "If AT-POINT is true it will pass the line number to mix test.
If TRACE runs tests with detailed reporting"
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-line (line-number-at-pos))
         (trace-flag (if trace
                         "--trace"
                       ""))
         (mix-file (concat (projectile-project-root) "mix.exs"))
         (default-directory (file-name-directory mix-file))
         (mix-env (concat "MIX_ENV=test ")))

    (if at-point
        (compile (format "%s mix test %s %s:%s" mix-env trace-flag current-file current-line))
      (compile (format "%s mix test %s %s" mix-env trace-flag current-file)))))

(defun my/mix-run-test-file ()
  "Run mix test over the current file."
  (interactive)
  (my/mix-run-test nil nil))

(defun my/mix-run-test-at-point ()
  "Run mix test at point."
  (interactive)
  (my/mix-run-test t nil))

(defun my/mix-run-tests-with-trace ()
  "Run mix test at point."
  (interactive)
  (my/mix-run-test nil t))

(with-eval-after-load 'elixir-ts-mode
  (define-key elixir-ts-mode-map (kbd "C-c C-t") 'my/mix-run-test-at-point)
  (define-key elixir-ts-mode-map (kbd "C-c t t") 'my/mix-run-tests-with-trace)
  (define-key elixir-ts-mode-map (kbd "C-c C-f") 'elixir-format)
  (define-key elixir-ts-mode-map (kbd "C-c h") 'mark-defun)
  (define-key elixir-ts-mode-map (kbd "M-a") 'treesit-beginning-of-defun)
  (define-key elixir-ts-mode-map (kbd "M-e") 'treesit-end-of-defun)
  (define-key eglot-mode-map (kbd "C-c i") 'consult-eglot-symbols)
  (add-hook 'elixir-ts-mode-hook #'rainbow-delimiters-mode)
  )

(use-package exunit
  :ensure t
  :config
  ;; Default environment wrapper
  (defun my/exunit-with-erl-flags (orig-fun &rest args)
    "Run ExUnit with custom ERL_FLAGS for increased scheduler count"
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "ERL_FLAGS" "-kernel +MIscs 2048")
      (setenv "MIX_ENV" "test")
      (message "Running tests with ERL_FLAGS=\"-kernel +MIscs 2048\" MIX_ENV=test")
      (apply orig-fun args)))

  (defun my/exunit-show-env-in-command (orig-fun &rest args)
    "Prepend environment variables to the displayed command"
    (let ((original-command (apply orig-fun args)))
      (format "ERL_FLAGS=\"-kernel +MIscs 2048\" MIX_ENV=test %s"
              original-command)))

  ;; Apply to all exunit commands
  (advice-add 'exunit-verify-all :around #'my/exunit-with-erl-flags)
  (advice-add 'exunit-verify-single :around #'my/exunit-with-erl-flags)
  (advice-add 'exunit-verify :around #'my/exunit-with-erl-flags)
  (advice-add 'exunit-rerun :around #'my/exunit-with-erl-flags)
  (advice-add 'exunit-build-command :around #'my/exunit-show-env-in-command)
  )

(provide 'init-elixir)
 ;;; init-elixir.el ends here
