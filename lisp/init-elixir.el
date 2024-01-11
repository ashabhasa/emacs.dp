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


;; (add-to-list 'eglot-server-programs '(elixir-mode  "~/dev/elixir/elixir-ls-gh/release_16_11_2023/language_server.sh"))

(add-to-list 'eglot-server-programs '(elixir-ts-mode  "~/dev/elixir/elixir-ls-gh/release_02_01_2024/language_server.sh"))

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


;; add treesitter support
(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex.git"))
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

;; (with-eval-after-load 'elixir-mode
;;   (define-key elixir-mode-map (kbd "C-c C-f") 'elixir-format)
;;   (define-key elixir-mode-map (kbd "C-c C-t") 'my/mix-run-test-at-point))

(with-eval-after-load 'elixir-ts-mode
  (define-key elixir-ts-mode-map (kbd "C-c C-t") 'my/mix-run-test-at-point)
  (define-key elixir-ts-mode-map (kbd "C-c C-f") 'elixir-format)  )


(provide 'init-elixir)
 ;;; init-elixir.el ends here
