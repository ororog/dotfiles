;; Initialize packages
(defun install-missing-packages (package-list)
  "install package if not installed"
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(setq-default package-archives
              '(("elpa" . "http://tromey.com/elpa/")
                ("gnu" . "http://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(install-missing-packages
 '(helm
   web-mode
   projectile
   helm-projectile
   enh-ruby-mode
   recentf-ext
   highlight-symbol
   exec-path-from-shell
   rubocop
   flycheck
   ))

(unless package-archive-contents
  (package-refresh-contents))

;; Global settings
(tool-bar-mode -1)
(setq-default show-trailing-whitespace t)
(show-paren-mode)
(set-background-color "Black")
(set-foreground-color "LightGray")
(set-cursor-color "Gray")
(set-frame-parameter nil 'alpha '(80 65))
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 2)
(column-number-mode t)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-_") 'text-scale-decrease)
(global-set-key (kbd "M--") 'text-scale-decrease)
(exec-path-from-shell-initialize)
(setq-default require-final-newline t)
(require 'highlight-symbol)
(highlight-symbol-mode t)
(setq-default highlight-symbol-idle-delay 0.3)
(prefer-coding-system 'utf-8)

;; Faces custmization
(defun describe-face-at-point ()
  "Show face name at cursor point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))
(set-face-background 'font-lock-warning-face "red")

;; Backup file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory nil)))

;; Settings for mac
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; Recentf
;; Save history and clean up automatically.
(require 'recentf-ext)
(when (require 'recentf nil t)
  (run-with-idle-timer 30 t 'recentf-save-list)
  (custom-set-variables
   '(recentf-max-saved-items 1000)
   '(recentf-exclude '(".recentf"))
   '(recentf-auto-cleanup 10)
   '(recentf-auto-save-timer))
  (recentf-mode 1))

;; Helm
(require 'helm-config)
(helm-mode 1)
(projectile-global-mode)
(helm-projectile-on)
(custom-set-variables
 '(helm-truncate-lines t)
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-candidate-number-limit 100)
 '(helm-for-files-preferred-list
   '(helm-source-recentf
     helm-source-projectile-files-list
     helm-source-buffers-list
     helm-source-files-in-current-dir))
 '(projectile-completion-system 'helm))
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-for-files)
(define-key global-map (kbd "C-x f") 'helm-find-files)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;; Ruby
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files.")
(autoload 'flycheck "flycheck" "Realtime ruby syntax checker.")
(autoload 'rubocop "rubocop" "Syntax check definition.")
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(defun ruby-mode-hooks ()
  "Hooks for ruby."
  (custom-set-variables
   '(ruby-insert-encoding-magic-comment nil)
   '(flycheck-checker 'ruby-rubocop)
   '(enh-ruby-deep-indent-paren nil)
   '(enh-ruby-hanging-indent-level 4)
   '(enh-ruby-hanging-paren-indent-level 4))
  (font-lock-add-keywords
   nil '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))
  (flycheck-mode t))
(add-hook 'enh-ruby-mode-hook 'ruby-mode-hooks)
(defun remove-enh-magic-comment ()
  (remove-hook 'before-save-hook 'enh-ruby-mode-set-encoding t))
(add-hook 'enh-ruby-mode-hook 'remove-enh-magic-comment)

;; Projectile
(custom-set-variables
 '(projectile-globally-ignored-file-suffixes '("png" "jpg"))
 '(projectile-sort-order 'recently-active))

;; web-mode
(defun web-mode-hooks ()
  "Hooks for web mode."
  (set-face-background 'web-mode-current-element-highlight-face "yellow")
  (custom-set-variables
   '(web-mode-enable-current-element-highlight t)
   '(web-mode-enable-auto-indentation nil)
   '(web-mode-code-indent-offset 8)'
   '(web-mode-enable-auto-closing t)))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-hook 'web-mode-hook 'web-mode-hooks)
