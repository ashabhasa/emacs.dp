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
;; (add-hook 'elixir-mode-hook 'eglot-ensure)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)


;; (add-to-list 'eglot-server-programs '(elixir-ts-mode  "~/dev/elixir/elixir-ls-gh/release_26_08_2024/language_server.sh"))
(add-to-list 'eglot-server-programs '(elixir-ts-mode  "~/dev/elixir/elixir-ls-gh/release_12_10_2024/language_server.sh"))

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
  ;; use dumb-jump for elixir
  ;; (add-to-list 'eglot-stay-out-of 'xref)
  ;; (when (maybe-require-package 'dumb-jump)
  ;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;;   (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;;   (setq dumb-jump-force-searcher 'rg))
  (add-hook 'elixir-ts-mode-hook #'rainbow-delimiters-mode)
  )

(provide 'init-elixir)
 ;;; init-elixir.el ends here
