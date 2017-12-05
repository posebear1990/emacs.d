(when (>= emacs-major-version 24)
  (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
	;; --- Auto-completion ---
	company
	;; --- Better Editor ---
	hungry-delete
	swiper
	counsel
	smartparens
	;; --- Major Mode ---
	js2-mode
	;; --- Minor Mode ---
	nodejs-repl
	exec-path-from-shell
	;; --- Themes ---
	atom-one-dark-theme
	;; solarized-theme
	) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
   (loop for pkg in my/packages
   when (not (package-installed-p pkg)) do (return nil)
   finally (return t)))

(unless (my/packages-installed-p)
   (message "%s" "Refreshing package database...")
   (package-refresh-contents)
   (dolist (pkg my/packages)
     (when (not (package-installed-p pkg))
       (package-install pkg))))

(require 'hungry-delete)
(global-hungry-delete-mode)

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
 (exec-path-from-shell-initialize))

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-linum-mode 1)

;; 更改光标的样式（不能生效，解决方案见第二集）
(setq-default cursor-type 'bar)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 140)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
    (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

;; 开启全局 Company 补全
(global-company-mode 1)

;; 关闭默认的备份文件,和文件自动保存功能
(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

(delete-selection-mode 1)

;; 默认全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 自动括号匹配
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 高亮当前行
(global-hl-line-mode 1)

;; 默认加载主题
(add-to-list 'my/packages 'atom-one-dark-theme)

(load-theme 'atom-one-dark t)


  (require 'smex) ; Not needed if you use package.el
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.

;; (global-set-key (kbd "M-x") 'smex)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)

(require 'smartparens-config)
(smartparens-global-mode t)


(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))


;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/workspace/emacs-org-mode"))

;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
