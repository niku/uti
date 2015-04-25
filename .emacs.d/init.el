(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle use-package)
(require 'use-package)

;;;
;;; デフォルトの設定
;;;

(setq inhibit-startup-screen t)         ; startupの画面を消す
(setq initial-scratch-message nil)      ; *scratch*のメッセージを消す
(menu-bar-mode 0)                       ; メニューバーを消す
(tool-bar-mode 0)                       ; ツールバーを消す
(set-scroll-bar-mode nil)               ; スクロールバーを消す
(setq visible-bell t)                   ; ビープ音を消す
(blink-cursor-mode 0)                   ; カーソルを点滅させない
(global-hl-line-mode 1)                 ; 現在行に色をつける
(transient-mark-mode t)                 ; リージョンをハイライト表示する
(setq kill-whole-line t)                ; C-kで改行までまとめて行をカットする
(delete-selection-mode 1)               ; BSで選択範囲を消す
(global-font-lock-mode t)               ; フォントロックモード (強調表示等) を有効にする
(global-auto-revert-mode t)             ; ファイルが変更されていたらバッファを再読み込みする
(setq-default indent-tabs-mode nil)     ; 字下げにタブを使用しない
(defalias 'yes-or-no-p 'y-or-n-p)       ; yes/noではなくy/nとする

(setq backup-inhibited t)    ; 保存時にバックアップファイルを作らない
(setq make-backup-files nil) ; *.~ とかのバックアップファイルを作らない
(setq auto-save-default nil) ; .#* とかのバックアップファイルを作らない
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; 保存時に行末の空白を削除する
(setq require-final-newline t)                           ; 最終行に必ず 1 行追加する
(setq next-line-add-newlines nil)                        ; バッファの最後でnewlineで新規行を追加するのを禁止する

(define-key global-map (kbd "C-h") 'delete-backward-char) ; C-hで一文字前を消す
(define-key global-map (kbd "C-m") 'newline-and-indent)   ; C-mで改行する

;; モードラインに時刻を表示する
(setq display-time-string-forms
      '((format "%s/%s(%s)%s:%s"
                month day dayname 24-hours minutes)))
(display-time-mode t)

;; 扱う文字のデフォルトを UTF-8 に設定する
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; OSX 用設定
(when (eq system-type 'darwin)
  ;; CommandとOptionを入れ替える
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  ;; フォント設定
  (set-face-attribute 'default nil :family "Ricty") ; 英語
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))) ; 日本語

;;;
;;; パッケージの設定
;;;

;; リストAPI
(el-get-bundle dash)
(use-package dash)

;; 文字列API
(el-get-bundle s)
(use-package s)

;; ファイルAPI
(el-get-bundle f)
(use-package f)

;; ハッシュAPI
(el-get-bundle ht)
(use-package ht)

;; エディタのテーマ(色)を設定する
(el-get-bundle solarized-theme)
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

;; emacsclient の接続を待ちうける
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; dired で直接書き換える
(use-package wdired
  :init
  (bind-keys :map dired-mode-map
             ("r" . wdired-change-to-wdired-mode)))

;; 開き括弧と閉じ括弧を対応表示する
(use-package paren
  :config
  (custom-set-variables
   '(show-paren-delay 0)
   '(show-paren-style 'mixed))
  (show-paren-mode t))

;; grep のかわりに ag を使う
(use-package grep
  :config
  (custom-set-variables
   '(grep-use-null-device nil))
  (grep-apply-setting 'grep-command "agg ")
  (grep-apply-setting 'grep-find-command "agg ")
  (grep-apply-setting 'grep-find-template "agg <R>")
  (grep-apply-setting 'grep-template "agg <X> <R> <F>")

  ;; この grep の方法はよくないと思うので変えたいなあ．
  ;; 「grep コマンドの最後に default-directory を付け足す」という方法がわかればいいんだけど．
  (defun grep-default-directory (pattern)
    (interactive "sPattern: ")
    (grep (format "agg %s %s" pattern default-directory)))
  :bind
  (("M-g M-d" . grep-default-directory)))

;; GUI Emacs での Path を shell と同期させる
(el-get-bundle exec-path-from-shell)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; 最近開いたファイルを記憶しておき，次に開くときに探しやすくする
(use-package recentf
  :config
  (custom-set-variables
   '(recentf-max-saved-items nil)
   '(recentf-exclude '(".recentf"))
   '(recentf-auto-cleanup 10))
  (recentf-mode 1))

(el-get-bundle recentf-ext)
(use-package recentf-ext)

;; 日本語をローマ字でインクリメンタルサーチする
(el-get-bundle migemo)
(use-package migemo
  :config
  (custom-set-variables
   '(migemo-command "cmigemo")
   '(migemo-options '("-q" "--emacs"))
   '(migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
   '(migemo-user-dictionary nil)
   '(migemo-regex-dictionary nil)
   '(migemo-coding-system 'utf-8-unix))
  (migemo-init))

;; 日本語入力を SKK で行う
(el-get-bundle ddskk)
(use-package ddskk
  :bind
  (("C-\\" . skk-mode))
  :init
  ;; ライブラリ読み込み前に設定しておかなければならない
  (custom-set-variables
   '(skk-server-host "localhost")
   '(skk-server-portnum 1178)
   '(skk-share-private-jisyo t))
  ;; 句読点を「．」「，」にする
  ;; (describe-variable 'skk-kutouten-type)
  ;; で `setq-default` を利用せよと書いてあったので従っている
  (setq-default skk-kutouten-type 'en))

;; helm
(el-get-bundle helm)
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-;" . helm-for-files)
   ("C-o" . helm-imenu)
   ("M-%" . helm-regexp)
   ("<help> a" . helm-apropos)))

;; git client
(el-get-bundle magit)
(use-package magit
  :init
  (delete 'Git vc-handled-backends)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind
  (("C-M-l" . magit-status)))

;; flycheck
(el-get-bundle flycheck)
(use-package flycheck
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(el-get-bundle flycheck-pos-tip
  :depends (flycheck popup))
(use-package flycheck-pos-tip
  :config
  '(custom-set-variables
    '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

;; projectile
(el-get-bundle projectile)
(use-package projectile
  :bind
  (("M-g M-r" . projectile-grep)))

;; yasnippet
(el-get-bundle yasnippet)
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; 補完機能を利用する
(el-get-bundle auto-complete)
(use-package auto-complete)

;; 一時的なウィンドウをポップアップで開く
(el-get-bundle popwin)
(use-package popwin
  :config
  (popwin-mode 1)
  ;; helm を popwin で開く
  (push '("\\*helm.*\\*" :regexp t) popwin:special-display-config)
  ;; quickrun を popwin で開く
  (push '("*quickrun*") popwin:special-display-config))

;; 今見ているソースが github にあればブラウザで開く
(el-get-bundle github-browse-file)
(use-package github-browse-file
  :bind
  (("C-M-g" . github-browse-file))
  :config
  (custom-set-variables
   '(github-browse-file-show-line-at-point t)))

;; 開いているバッファを簡単に実行して結果を得る
(el-get-bundle quickrun)
(use-package quickrun
  :bind
  (("C-c q r" . quickrun)))

;; twitter クライアント
(el-get-bundle twittering-mode)
(use-package twittering-mode
  :config
  (custom-set-variables
   '(twittering-use-master-password t)
   '(twittering-status-format "%i %s %p: %t")
   '(twittering-default-show-replied-tweets t)
   '(twittering-use-native-retweet t))
  ;; twitter client 名を変える
  (setq twittering-oauth-consumer-key (base64-decode-string "Q2tuVklCTUxVRHdIN01BOXg0V0huZw=="))
  (setq twittering-oauth-consumer-secret (base64-decode-string "NEpWVWhTVk4zVGJSeXZOUnZuakJ3YlpqdUF0RUV6UzhqWHpGWlhma1U=")))

;;;
;;; 言語別設定
;;;

;; 各メジャーモードが対応しているファイル名は以下のS式を評価すると得られる
;; ruby-mode の例
;; (let ((mode 'ruby-mode))
;;   (->> auto-mode-alist
;;        (-filter (lambda(cons) (eq (cdr cons) mode)))
;;        (-map 'car)))
;; => ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")

;;; Markdown
(el-get-bundle markdown-mode)
(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode))
  :commands
  (markdown-mode))

;;; Ruby
(use-package ruby-mode
  :config
  (custom-set-variables
   ;; Rubyコードの1行目にマジックコメントを挿入しない
   '(ruby-insert-encoding-magic-comment nil)))

;; M-x inf-ruby で REPL を起動する
(el-get-bundle inf-ruby)
(use-package inf-ruby
  :config
  (custom-set-variables
   '(inf-ruby-default-implementation "pry")
   '(inf-ruby-eval-binding "Pry.toplevel_binding")))

;; `class` や `if` などを書くと `end` を補完してくれ，
;; `(` を書くと `)`，`{` を書くと `}` を補完してくれる
;;
;; どんな文字が補完されるか知るには
;; - ruby-electric-simple-keywords-re
;; - ruby-electric-matching-delimeter-alist
;; を見るとよい
(el-get-bundle ruby-electric)
(use-package ruby-electric)

;; end に対応する class や if を示してくれる
;;
;; どんなものに対応してくれるか知るには
;; - ruby-block-keyword-list
;; を見るとよい
(el-get-bundle ruby-block)
(use-package ruby-block
  :init
  (defun ruby-mode-hooks-for-ruby-block-mode ()
    (custom-set-variables
     '(ruby-block-highlight-toggle t))
    (ruby-block-mode t))
  (add-hook 'ruby-mode-hook 'ruby-mode-hooks-for-ruby-block-mode))

;;; JavaScript
(use-package js-mode
  :mode
  (("\\.jshintrc\\'" . js-mode)))

;;; Elixir
(el-get-bundle elixir)
(use-package elixir)

;; Elixir の mix や iex などを Emacs から使えるようにしてくれる
(el-get-bundle alchemist)
(use-package alchemist)

;;; TypeScript
(el-get-bundle tss
  :depends
  (auto-complete json-mode log4e yaxception))
(use-package typescript-mode
  :mode
  (("\\.ts\\'" . typescript-mode)))
(use-package tss
  :config
  (tss-config-default))
