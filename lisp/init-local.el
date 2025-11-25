;;; init-local.el --- Load custom modification
;;; Commentary:
;;; Code:
;;; Fonts setup:


;; (set-face-attribute 'default nil
;;                     :family "PragmataPro Mono Liga"
;;                     :height 150
;;                     :weight 'regular)
;; (set-face-attribute 'default nil
;;                     :family "PragmataPro Mono Liga"
;;                     :height 130
;;                     :weight 'regular)


;; on lg 27
;; (set-face-attribute 'default nil :font "PragmataPro-13:antialias=true:hinting=true:autohint=false:hint=3")
;; (set-face-attribute 'default nil :font "Aporetic Sans Mono-13:antialias=true:hinting=true:autohint=false:hint=3")

;; orginal mac book
;; (set-face-attribute 'default nil :font "PragmataPro-15:antialias=false:hinting=true:autohint=false:hint=3")

;; on Dell
(set-face-attribute 'default nil
                    :family "Aporetic Sans Mono"
                    :height 140
                    :weight 'regular
                    )
(set-face-attribute 'default nil :font "Aporetic Sans Mono-14:antialias=true:hinting=true:autohint=false:hint=3")


;; use modus operandi theme
(package-install 'modus-themes)
(package-install 'ef-themes)

;; (initial-scratch-message nil)
;; (color-theme-sanityinc-tomorrow-night)
;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
;; (load-theme 'ef-day :no-confirm)
(load-theme 'ef-winter :no-confirm)

(global-set-key (kbd "M-w") 'easy-kill)

(global-set-key (kbd "M-j") 'join-lines)

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
(global-set-key (kbd "M-o") 'switch-window)
(global-set-key (kbd "C-c s") 'mark-sexp)

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
(global-set-key (kbd "C-x f") 'arber/open-file-at-cursor)
(defun xah-copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))

(global-set-key (kbd "C-x y b")  #'xah-copy-file-path)


;; avy configuration


;;Replace with your package manager or help library of choice

(require 'avy)

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(global-set-key (kbd "C-j") 'avy-goto-char-timer)
(setq avy-timeout-seconds 1)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)
(setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)


(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)


;; end avy configuration


;; isearch extensions
;; from protosillas

(defun arber/isearch-region (&optional not-regexp no-recuresive-edit)
  "If the region is active make it the default search string"
  (interactive "P\np")
  (when (use-region-p)
    (let ((search (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
      (message "arber/ir: %s %d %d" search (region-beginning) (region-end))
      (setq deactivate-mark t)
      (isearch-yank-string search))))

(advice-add 'isearch-forward-regexp :after 'arber/isearch-region)
(advice-add 'isearch-forward :after 'arber/isearch-region)
(advice-add 'isearch-backward-regexp :after 'arber/isearch-region)
(advice-add 'isearch-backward :after 'arber/isearch-region)

;; yas
(when (maybe-require-package 'yasnippet)
  (require-package 'yasnippet-snippets)
  (require 'yasnippet)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'conf-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  (add-hook 'snippet-mode-hook #'yas-minor-mode)
  )

(defun disable-new-line () (set (make-local-variable 'require-final-newline) nil))

(add-hook 'elixir-ts-mode-hook #'disable-new-line)
(add-hook 'snippet-mode-hook #'disable-new-line)


;; utilities to move between windows
(global-set-key [M-s-left] 'windmove-left)          ; move to left window
(global-set-key [M-s-right] 'windmove-right)        ; move to right window
(global-set-key [M-s-up] 'windmove-up)              ; move to upper window
(global-set-key [M-s-down] 'windmove-down)          ; move to lower window

;; utility to copy buffer file name
(defun filename ()
  "Copy the full path of the current buffer to the kill ring."
  (interactive)
  (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; magit utilities
(global-set-key (kbd "C-x m") 'magit-show-refs)          ; show refs in other buffer

(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "M-<up>") 'windmove-swap-states-up)
(global-set-key (kbd "M-<right>") 'windmove-swap-states-right)
(global-set-key (kbd "M-<left>") 'windmove-swap-states-left)
(global-set-key (kbd "M-<down>") 'windmove-swap-states-down)


;; add treesitter support for some languages
(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex.git")
        (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
        (php "https://github.com/tree-sitter/tree-sitter-php")
        )
      )

;; typescript configuration
;; I'm not sure why this is needed, but it throws an error if I remove it
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)

(add-to-list 'eglot-server-programs
             '((typescript-mode) "typescript-language-server" "--stdio"))


(eval-after-load "em-term"
  '(progn
     (add-to-list 'eshell-visual-subcommands '("git" "diff" "help" "log" "show"))
     (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))))

(unless (package-installed-p 'eshell-prompt-extras)
  (package-install 'eshell-prompt-extras))
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(unless (package-installed-p 'el-feed)
  (package-install 'elfeed))

(setq elfeed-feeds
      '(("https://www.theatlantic.com/feed/all" news atlantic)
        ("https://www.theguardian.com/uk/rss" news guardian)
        ("https://www.ft.com/news-feed?format=rss" finance ft)
        ("https://www.okmij.org/ftp/rss.xml" tech okmij)
        ("https://www.thecipherbrief.com/feeds/feed.rss" new cpb)
        ("https://lrb.co.uk/feeds/rss" books literature lrb)
        ))


(when (maybe-require-package 'undo-tree)
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package nerd-icons
  :ensure t)


;; use icons in dired
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; sql formatter
(setq sqlformat-command 'pgformatter)
;; Optional additional args
(setq sqlformat-args '("-s2" "-g"))

;; consult
(global-set-key [remap projectile] 'consult-project-buffer)
;; end consult
;; open line below
(defun arb-open-line-above (&optional arg)
  "Open line above cursor and move cursor to it."
  (interactive "*p")
  (move-beginning-of-line nil)
  (newline-and-indent arg)
  (forward-line -1)
  (indent-according-to-mode))

(defun arb-open-line-below (&optional arg)
  "Open line below cursor and move cursor to it."
  (interactive "*p")
  (move-end-of-line nil)
  (newline arg)
  (forward-line 0)
  (indent-according-to-mode))

(global-set-key (kbd "C-m") 'arb-open-line-below)
(global-set-key (kbd "C-o") 'arb-open-line-below)
(global-set-key (kbd "C-i") 'arb-open-line-above)
;; install define-word
(package-install 'define-word)
(global-set-key (kbd "M-#") 'define-word-at-point)

;; magit utilities
(defun ash/merge-current-to-branch (target-branch)
  "Merge current branch into TARGET-BRANCH."
  (interactive (list (magit-read-branch "Target branch to merge into")))
  (let ((current-branch (magit-get-current-branch)))
    (when (string= current-branch target-branch)
      (user-error "Cannot merge branch into itself"))
    (condition-case err
        (progn
          ;; 1. git switch target-branch
          (magit-checkout target-branch)
          ;; 2. git remote update -p
          (magit-run-git "remote" "update" "-p")
          ;; 3. git merge --ff-only @{u} (only if upstream exists)
          (when (magit-get-upstream-branch)
            (magit-run-git "merge" "--ff-only" "@{u}"))
          ;; 4. git merge - (merge previous branch)
          (magit-merge-plain current-branch)
          ;; Check for conflicts
          (if (magit-anything-unmerged-p)
              (message "Merge conflicts detected in %s. Resolve conflicts and commit." target-branch)
            (message "Successfully merged %s into %s" current-branch target-branch)))
      (error
       (message "Merge operation failed: %s" (error-message-string err))))))


(defun ash/update-current-branch ()
  "Update current branch to its upstream."
  (interactive)
  (let ((current-branch (magit-get-current-branch))
        (upstream-branch (magit-get-upstream-branch)))
    (cond
     ;; Check if we're in a repository
     ((not (magit-toplevel))
      (message "Not in a git repository"))
     ;; Check if we have a current branch (not in detached HEAD)
     ((not current-branch)
      (message "Not on any branch (detached HEAD state)"))
     ;; Check if upstream is configured
     ((not upstream-branch)
      (message "No upstream configured for branch '%s'. Set upstream first." current-branch))
     ;; All checks passed, proceed with update
     (t
      (message "Updating %s to %s..." current-branch upstream-branch)
      (condition-case err
          (progn
            ;; Fetch all remotes with prune
            (magit-run-git "remote" "update" "-p")
            (message "Fetched from all remotes")
            ;; Try fast-forward merge
            (magit-run-git "merge" "--ff-only" "@{u}")
            (message "Successfully updated %s to %s" current-branch upstream-branch))
        ;; Handle specific error types
        (magit-git-error
         (message "Git error while updating %s: %s" current-branch (error-message-string err)))
        (user-error
         (message "Cannot fast-forward %s: branch has diverged from %s" current-branch upstream-branch))
        (error
         (message "Failed to update %s: %s" current-branch (error-message-string err))))))))

(global-set-key (kbd "C-x v b u") #'ash/update-current-branch)
(global-set-key (kbd "C-x v b m") #'ash/merge-current-to-branch)

(provide 'init-local)
 ;;; init-local.el ends here
