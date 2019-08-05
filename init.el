;;; package --- summary: Benjamin Killeen's Emacs config file.
;;
;;; Comentary: various sections from various sources
;;
;;; Code:

;; should be at top
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "bc75dfb513af404a26260b3420d1f3e4131df752c19ab2984a7c85def9a2917e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(font-latex-user-keyword-classes
   (quote
    (("think"
      (("think" "{"))
      font-latex-italic-face command))))
 '(package-selected-packages
   (quote
    (auto-package-update ensime elpy elpygen py-autopep8 better-defaults zenburn-theme racket-mode evil-visual-mark-mode sml-mode yasnippet-snippets jedi-direx helm-ag nyan-mode smart-mode-line-atom-one-dark-theme smart-mode-line-powerline-theme smart-mode-line doom-modeline arjen-grey-theme abyss-theme dracula-theme magit-popup magit highlight-numbers kaolin-themes sphinx-doc irony pov-mode markdown-mode js2-mode ein anaconda-mode cython-mode zotelo synonyms s-buffer pandoc-mode omnisharp olivetti minesweeper mediawiki icicles helm git fireplace exec-path-from-shell chess auto-complete-auctex auctex)))
 '(safe-local-variable-values
   (quote
    ((TeX-command-extra-options . "-shell-escape")
     (tex-master . "vanesh"))))
 '(send-mail-function (quote mailclient-send-it)))


;; emacs load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; adjust the background of the loaded theme
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Using Melpa
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; themes and appearance
(load-theme 'zenburn t)

;; startup
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)

;; change the mode-line
(use-package smart-mode-line)
(setq sml/theme 'respectful)
(sml/setup)

;; other highlighting
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; overriding keybindings
(use-package bind-key)

;; miscellaneous keybindings
(bind-keys*
 ;; ("M-m" . menu-bar-open)
 ("C-c a" . auto-fill-mode)
 ("C-c o" . olivetti-mode)
 ("C-x a" . align-regexp)
 ("C-c r" . replace-string)
 ("C-c C-r" . replace-regexp)
 ("C-c d" . delete-trailing-whitespace)
 ("C--"   . undo)
 ("C-M-f" . forward-whitespace)
 ;; ("M-," . previous-buffer)
 ("C-M-p" . comint-previous-input)
 ("C-M-n" . comint-next-input)
 ("M-s M-s" . rgrep)
 ("C-c C-l" . global-linum-mode))

(bind-key* "C-c l" '(lambda() (interactive) (load-file "~/.emacs.d/init.el")
                      (unless (display-graphic-p (selected-frame))
                        (set-face-background 'default "unspecified-bg" (selected-frame)))))
(bind-key* "C-M-b" '(lambda() (interactive) (forward-whitespace -1)))
(bind-key* "C-c e" '(lambda() (interactive) (find-file "~/.emacs.d/init.el")))
(bind-key* "C-M-k" '(lambda() (interactive) (kill-line 0)))
(bind-key* "C-q" '(lambda() (interactive) (switch-to-buffer (other-buffer))))
(bind-key* "C-c c" '(lambda() (interactive) (shell-command-on-region
                                             (region-beginning)
                                             (region-end)
                                             "pbcopy" (messages-buffer))))



(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))
(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        ($i 1))
    (while (<= $i n)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq $i n)))
      (setq $i (1+ $i)))))
(bind-key* "M-p" '(lambda() (interactive) (xah-backward-block) (next-line)))
(bind-key* "M-n" '(lambda() (interactive) (xah-forward-block) (previous-line)))

;; deal with tabs
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq python-indent-offset 2)
(setq sml-indent-level 2)
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Toggle transparecy when in graphical mode with "C-c t"
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(35 . 50) '(100 . 100)))))
(bind-key* "C-c t" 'toggle-transparency)

(defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
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

;; remap C-a to `smarter-move-beginning-of-line'
(bind-key* [remap move-beginning-of-line]
          'smarter-move-beginning-of-line)

;; Miscellaneous variable assignments
(setq-default fill-column 79)

;; Haskell mode
(use-package haskell-mode)              ;TODO: check


;; LaTeX/AucTeX stuff:
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)                   ;compile to pdf
'(reftex-use-external-file-finders t)
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;;
;; TeXcount setup for TeXcount version 2.3 and later
;;
(defun texcount ()
  (interactive)
  (let*
    ( (this-file (buffer-file-name))
      (enc-str (symbol-name buffer-file-coding-system))
      (enc-opt
        (cond
          ((string-match "utf-8" enc-str) "-utf8")
          ((string-match "latin" enc-str) "-latin1")
          ("-encoding=guess")
      ) )
      (word-count
        (with-output-to-string
          (with-current-buffer standard-output
            (call-process "texcount" nil t nil "-0" enc-opt this-file)
    ) ) ) )
    (message word-count)
) )
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-cw" 'texcount)))
(add-hook 'latex-mode-hook (lambda () (define-key latex-mode-map "\C-cw" 'texcount)))

;; Emacs appearance customizations (mostly fonts):
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#ec37d8"))))
 '(font-latex-italic-face ((t (:inherit italic :foreground "brightred"))))
 '(font-latex-math-face ((t (:foreground "cyan"))))
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face :height 1.1))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face :height 1.1))))
 '(font-latex-sectioning-5-face ((t (:foreground "brightyellow" :weight bold))))
 '(font-latex-sedate-face ((t (:foreground "color-29"))))
 '(font-latex-string-face ((t (:foreground "color-173"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "color-129" :slant normal :weight normal))))
 '(mouse ((t nil))))

;; Backing up files (might be working?)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(put 'downcase-region 'disabled nil)

;; Automatic minor modes, from stack exchange:
(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")
(defun enable-minor-mode-based-on-extension ()
    "check file name against auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
    (when buffer-file-name
      (let ((name buffer-file-name)
            (remote-id (file-remote-p buffer-file-name))
            (alist auto-minor-mode-alist))
        ;; Remove backup-suffixes from file name.
        (setq name (file-name-sans-versions name))
        ;; Remove remote file name identification.
        (when (and (stringp remote-id)
                   (string-match-p (regexp-quote remote-id) name))
          (setq name (substring name (match-end 0))))
        (while (and alist (caar alist) (cdar alist))
          (if (string-match (caar alist) name)
              (funcall (cdar alist) 1))
          (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)

;; Olivetti mode for various docs:
;; (defvar olivetti-body-width 82)
;; (setq auto-minor-mode-alist
;;       (cons '("\\.txt\\'" . olivetti-mode)
;;             auto-minor-mode-alist))
;; (setq auto-minor-mode-alist
;;       (cons '("\\.tex\\'" . olivetti-mode)
;;             auto-minor-mode-alist))

;; ;; autofill mode, same thing
;; (setq auto-minor-mode-alist
;;       (cons '("\\.txt\\'" . auto-fill-mode)
;;             auto-minor-mode-alist))
;; (setq auto-minor-mode-alist
;;       (cons '("\\.tex\\'" . auto-fill-mode)
;;             auto-minor-mode-alist))


;; Auto-complete mode:
(use-package auto-complete)
(ac-config-default)
(ac-set-trigger-key "TAB")
(use-package auto-complete-auctex)

;; Better C stuff
(add-hook 'c-mode-hook 'auto-complete-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; cause bracketed paste problem:
;; (global-set-key (kbd "M-[") 'windmove-left)
;; (global-set-key (kbd "M-]") 'windmove-right)
;; (global-set-key (kbd "M-[") 'other-window)
(global-set-key (kbd "M-]") 'other-window)
(bind-key* "C-<return>" 'other-window)

;; python config
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")
(use-package py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=100"))
(setq py-autopep8-options '("--indent-size=2"))
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
(add-hook 'elpy-mode-hook (lambda ()
                            (bind-key* "C-x M-." 'elpy-goto-definition-other-window)
                            (bind-key* "M-," 'pop-tag-mark))

;; auto-docstrings in python
(use-package sphinx-doc)
(add-hook 'python-mode-hook (lambda ()
                              (sphinx-doc-mode t)
                              (bind-key* "C-c C-s" 'sphinx-doc)
                              (setq paragraph-start (concat paragraph-start ))))


(use-package yasnippet)
(yas-global-mode 1)
(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
     (nr 0)
         (formatted-args
      (mapconcat
       (lambda (x)
         (concat "   " (nth 0 x)
             (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
             (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
       args
       indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
          (list "" "Args:" formatted-args)
          indent)
       "\n"))))
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)


;; pandoc mode
(add-hook 'markdown-mode-hook 'pandoc-mode)

(defun mg-TeX-insert-single-quote (force)
  "Insert the appropriate quotation marks for TeX.
Inserts ` or ' depending on the context.  With prefix argument
FORCE, always inserts ' characters."
  (interactive "*P")
  (if (or force
      ;; Do not insert TeX quotes in verbatim, math or comment constructs.
      (and (fboundp 'font-latex-faces-present-p)
           (font-latex-faces-present-p '(font-latex-verbatim-face
                         font-latex-math-face
                         font-lock-comment-face))
           (font-latex-faces-present-p '(font-latex-verbatim-face
                         font-latex-math-face
                         font-lock-comment-face)
                       (1- (point))))
      (texmathp)
      (and (TeX-in-comment) (not (eq major-mode 'doctex-mode))))
      (self-insert-command (prefix-numeric-value force))
    (TeX-update-style)
    (let* ((open-quote "`")
       (close-quote "'"))
      (insert (cond ((bobp)
             open-quote)
            ((= (preceding-char) (string-to-char TeX-esc))
             ?\')
            ((= (preceding-char) ?\")
             ?')
            ((save-excursion
               (forward-char (- (length open-quote)))
               (looking-at (regexp-quote open-quote)))
             (delete-char (- (length open-quote)))
             ?')
            ((save-excursion
               (forward-char (- (length close-quote)))
               (looking-at (regexp-quote close-quote)))
             (delete-char (- (length close-quote)))
             ?')
            ((save-excursion
               (forward-char -1)
               (looking-at "[ \t\n]\\|\\s("))
             open-quote)
            (t
             close-quote))))))

(eval-after-load "tex"
  '(define-key LaTeX-mode-map (kbd "'") 'mg-TeX-insert-single-quote))

;; TODO: deal with this
;; POV-Ray mode stuff
(add-to-list 'load-path "~/.emacs.d/elpa/pov-mode-20161115.743/pov-mode.el")
(autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
(add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))

;; Create a tags file
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; Auto refresh of the tags file
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))
(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(fset 'full-points
      "\C-[xreplace-regexp\C-m /\\([0-9]+\\)\C-m \\1/\\1\C-m")
(defun my-text-mode-hook ()
  ;; enable the stuff you want for .txt files here
  (bind-key "C-c C-f" 'full-points)
  )
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; magit things
(use-package magit-popup
  :ensure t ; make sure it is installed
  :demand t ; make sure it is loaded
  )
(bind-key* "C-c m" 'magit-status)

;; Create a tags file
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; Auto refresh of the tags file
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))
(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))


;; java shit
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)))

;; scala shit
(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package auto-package-update)

;;; init.el ends here
