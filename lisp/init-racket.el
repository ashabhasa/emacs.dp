;;; init-racket.el --- Support the Racket language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'racket-mode)
  (unless (package-installed-p 'racket-mode)
    (package-install 'racket-mode)))

(provide 'init-racket)
;; init-racket.el ends here
