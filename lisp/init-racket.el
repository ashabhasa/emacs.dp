;;; init-racket.el --- Support the Racket language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'racket-mode)
  (unless (package-installed-p 'racket-mode)
    (package-install 'racket-mode)))


(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)
(add-hook 'racket-xp-mode-hook
          (lambda ()
            (remove-hook 'pre-redisplay-functions
                         #'racket-xp-pre-redisplay
                         t)))

(provide 'init-racket)
;; init-racket.el ends here
