;;; init-local.el --- Load custom modification
 ;;; Commentary:
 ;;; Code:
 ;;; Fonts setup:


;; (set-face-attribute 'default nil
;; :family "PragmataPro"
;; :height 175
;; :weight 'regular)

;; (set-face-attribute 'fixed-pitch nil
;; :family "PragmataPro"
;; :height 175
;; :weight 'regular)

(set-face-attribute 'default nil :height 140)

(global-set-key (kbd "M-w") 'easy-kill)

(global-set-key (kbd "M-j") 'join-lines)

;; disable vertical line indicator
(add-hook 'prog-mode-hook (lambda () (global-display-fill-column-indicator-mode  -1)))

;; enable origami folding
(add-hook 'prog-mode-hook 'origami-mode)

;; bind M-? to x-find-ref
(global-set-key (kbd "M-?") #'xref-find-references)

(setq-default line-spacing nil)

(setq mac-allow-anti-aliasing 1)
;; Better scrolling

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
;; Save file backups in .emacs-backup
(setq backup-directory-alist `(("." . "~/.emacs-backup")))
 ;;; Make Emacs look in Stack/Cabal directory for binaries
(let ((my-local-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat my-local-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-local-path))
(let ((my-stack-path (expand-file-name "/usr/local/bin")))
  (setenv "PATH" (concat my-stack-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-stack-path))

;;Copied from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun arber/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
   Move point to the first non-whitespace character on this line.
   If point is already ther, move to the beginning of the line.
   Effectively toggle between the first non-whitespace charcater and
   the beginning of the line.
   If ARG is not nil or 1, move forward ARG -1 lines first.
   If point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; Remap C-a to `arber/smarter-move-beginning-of-line`
(global-set-key [remap move-beginning-of-line] 'arber/smarter-move-beginning-of-line)
;; comments
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
;; org mode
(require-package 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("•"))
(require-package 'easy-kill)
(cua-mode -1)
;; Use easy-kill in place of kill-ring-save
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap whole-line-or-region-kill-ring-save] 'easy-kill)
;; Use easy-mark in place of mark-sexp
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap counsel-apropos] 'apropos-command)
(setq create-lockfiles nil)
(defun join-lines (n)
  "Join N lines."
  (interactive "p")
  (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
    (dotimes (_ (abs n))
      (delete-indentation (natnump n)))))
(global-set-key (kbd "C-^") 'join-lines)
;; Enable silver searcher for fast search
(require-package 'ag)

;; bind M-i to imenu
(global-set-key (kbd "M-m") 'consult-imenu)

;; --- Arber
;; xml configuration
;;(require 'init-nxml)
(require 'nxml-mode)
(define-key nxml-mode-map (kbd "C-c m f") 'sanityinc/pp-xml-region)
(define-key nxml-mode-map (kbd "C-c m b") 'sanityinc/tidy-buffer-xml)
 ;;; (define-key nxml-mode-map (kbd "C-M d")   'nxml-down-element)
;; Enable hiding and showing of xml nodes using C-c h short cut
(require 'hideshow)
(require 'sgml-mode)
;;(require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)
;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
;; Where am i
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path from http://www.emacswiki.org/emacs/NxmlMode."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                 ;; beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))
(add-hook 'which-func-functions 'nxml-where)
;; end of xml configuration


;; this snippet was taken from http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html
(defun arber/open-file-at-cursor ()
  "Open the file path under cursor.
 If there is text selection, uses the text selection for path.
 If the path starts with “http://”, open the URL in browser.
 Input path can be {relative, full path, URL}.
 Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
 If path does not have a file extension, automatically try with “.el” for elisp files.
 This command is similar to `find-file-at-point' but without prompting for confirmation.
 URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
 Version 2020-10-17"
  (interactive)
  (let* (
         ($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x )
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "#" $path )
            (let (
                  ( $fpath (substring $path 0 (match-beginning 0)))
                  ( $fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char (point-min))
                    (search-forward $fractPart ))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char (point-min))
                      (forward-line (1- $line-num)))
                  (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
                  (find-file $path ))))))))))

(global-set-key (kbd "C-c f")  #'arber/open-file-at-cursor)


(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1))

(provide 'init-local)
 ;;; init-local.el ends here
