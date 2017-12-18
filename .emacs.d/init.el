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
   scss-mode
   js2-mode
   magit
   jedi
   ))

(unless package-archive-contents
  (package-refresh-contents))

;; Global settings
(electric-indent-mode -1)
(tool-bar-mode -1)
(global-unset-key "\C-z")
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(enh-ruby-deep-indent-paren nil)
 '(enh-ruby-hanging-indent-level 4)
 '(enh-ruby-hanging-paren-indent-level 4)
 '(flycheck-checker (quote ruby-rubocop) t)
 '(helm-candidate-number-limit 100)
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-recentf helm-source-projectile-files-list helm-source-buffers-list helm-source-files-in-current-dir)))
 '(helm-truncate-lines t t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(package-selected-packages
   (quote
    (company-jedi jedi web-mode scss-mode ruby-block rubocop recentf-ext magit js2-mode highlight-symbol helm-projectile flycheck exec-path-from-shell enh-ruby-mode)))
 '(projectile-completion-system (quote helm))
 '(projectile-globally-ignored-file-suffixes (quote ("png" "jpg")))
 '(projectile-sort-order (quote recently-active))
 '(recentf-auto-cleanup 10)
 '(recentf-auto-save-timer nil)
 '(recentf-exclude (quote (".recentf")))
 '(recentf-max-saved-items 1000)
 '(ruby-insert-encoding-magic-comment nil)
 '(scss-compile-at-save nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 2))
(add-to-list 'projectile-globally-ignored-directories "tmp")
(add-to-list 'projectile-globally-ignored-directories "log")
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
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(defun ruby-mode-hooks ()
  "Hooks for ruby."
  (custom-set-variables
   '(ruby-insert-encoding-magic-comment nil)
   '(flycheck-checker 'ruby-rubocop)
   '(enh-ruby-deep-indent-paren nil)
   '(enh-ruby-hanging-indent-level 4)
   '(enh-ruby-hanging-paren-indent-level 4))
  (flycheck-mode 1)
  (font-lock-add-keywords
   nil '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))
  (font-lock-add-keywords
   nil '((" \\{2,\\}" 1 font-lock-warning-face t)) t))
(add-hook 'enh-ruby-mode-hook 'ruby-mode-hooks)
(defun remove-enh-magic-comment ()
  (remove-hook 'before-save-hook 'enh-ruby-mode-set-encoding t))
(add-hook 'enh-ruby-mode-hook 'remove-enh-magic-comment)

;; git setting


;; web-mode
(defun web-mode-hooks ()
  "Hooks for web mode."
  (set-face-background 'web-mode-current-element-highlight-face "yellow")
  (font-lock-add-keywords
   'nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)) t)
  (custom-set-variables
   '(web-mode-enable-current-element-highlight t)
   '(web-mode-enable-auto-indentation nil)
   '(web-mode-code-indent-offset 2)
   '(web-mode-markup-indent-offset 2)
   '(web-mode-enable-auto-closing t)))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-hook 'web-mode-hook 'web-mode-hooks)

;; scss
(autoload 'scss-mode "scss-mode")
(defun scss-mode-hooks ()
  "Hooks for scss mode."
  (custom-set-variables
   '(css-indent-offset 2)
   '(scss-compile-at-save nil))
  (rubocop-mode nil)
  (flycheck-mode t))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode-hook 'scss-mode-hooks)

;; js2-mode
(defun js2-mode-hooks ()
  "Hooks for js2 mode."
  (font-lock-add-keywords
   nil '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))
  (custom-set-variables
   '(js2-basic-offset 2)))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'js2-mode-hooks)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ispell
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; magit
(require 'magit)
(setq ediff-split-window-function 'split-window-horizontally)
(defun ediff-mode-hooks ()
  (set-frame-parameter initial-frame-alist 'alpha '(100 100)))
(defun ediff-quit-hooks ()
  (set-frame-parameter initial-frame-alist 'alpha '(85 65)))
(add-hook 'ediff-mode-hook 'ediff-mode-hooks)
(add-hook 'magit-ediff-quit-hook 'ediff-quit-hooks)

;; python
(defun python-mode-hooks()
  (setq-default indent-tabs-mode nil)
  (setq-default python-indent 2)
  (setq-default indent-level 2)
  (setq-default tab-width 2))
(add-hook 'python-mode-hook 'python-mode-hooks)

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
