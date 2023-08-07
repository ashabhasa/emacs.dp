;;; init-elixir.el --- Support for the Elixir language
 ;;; Commentary:
 ;;; Code:

(require 'eglot)

(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

(display-fill-column-indicator-mode -1)
;; This is optional. It automatically runs `M-x eglot` for you whenever you are in `elixir-mode`
(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(elixir-mode  "~/dev/elixir/elixir-ls-gh/release_04_07_2023/language_server.sh"))

(unless (package-installed-p 'exunit)
  (package-install 'exunit))

(when (maybe-require-package 'exunit)
  (add-hook 'elixir-mode-hook 'exunit-mode))

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(unless (package-installed-p 'yasnippet-snippets)
  (package-install 'yasnippet-snippets)
  (maybe-require-package 'yasnippet-snippets))

(when (maybe-require-package 'yasnipet)
  (add-hook 'prog-mode 'yas-minor-mode)
  (add-hook 'conf-mode 'yas-minor-mode)
  (add-hook 'text-mode 'yas-minor-mode)
  (add-hook 'snippet-mode 'yas-minor-mode)
  (require-package 'yasnippet-snippets)
  )


;; run mox test from emacs
;; copied from https://dev.to/erickgnavar/minimal-setup-for-elixir-development-in-emacs-5k4
(defun my/mix-run-test (&optional at-point)
  "If AT-POINT is true it will pass the line number to mix test."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-line (line-number-at-pos))
         (mix-file (concat (projectile-project-root) "mix.exs"))
         (default-directory (file-name-directory mix-file))
         (mix-env (concat "MIX_ENV=test ")))

    (if at-point
        (compile (format "%s mix test %s:%s" mix-env current-file current-line))
      (compile (format "%s mix test %s" mix-env current-file)))))

(defun my/mix-run-test-file ()
  "Run mix test over the current file."
  (interactive)
  (my/mix-run-test nil))

(defun my/mix-run-test-at-point ()
  "Run mix test at point."
  (interactive)
  (my/mix-run-test t))

(with-eval-after-load 'elixir-mode
  (define-key elixir-mode-map (kbd "C-c C-f") 'elixir-format)
  (define-key elixir-mode-map (kbd "C-c C-t") 'my/mix-run-test-at-point))


(provide 'init-elixir)
 ;;; init-elixir.el ends here
