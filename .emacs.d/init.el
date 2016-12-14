; (package-initialize)

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
;;; 便利なAPIを設定の記述に使いたいので先に読み込んでおく
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

;;;
;;; デフォルトの設定
;;;

;; custom-set-variable の設定を書き出すファイルを init.el から移す
(let ((custom-set-variable-file (expand-file-name "custom-set-variables.el" user-emacs-directory)))
  (when (f-readable? custom-set-variable-file)
    (load custom-file)))

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

;; エディタのテーマ(色)を設定する
(el-get-bundle solarized-emacs)
(use-package solarized-theme
  :config
  (custom-set-variables
   ;; org-mode の見出し行の文字の大きさを変えない
   '(solarized-scale-org-headlines nil))
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

;; grep の結果を表示しているバッファで C-c C-p を押して直接編集する
(el-get-bundle wgrep)
(use-package wgrep)

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
   '(recentf-exclude '(".recentf")))
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

;;; eshell
(use-package eshell
  :config
  (custom-set-variables
   ;; ファイル補完時に大文字と小文字を区別しない
   '(eshell-cmpl-ignore-case t)
   ;; 履歴を最大限保持する
   ;; maximum value of integer in emacs(64bit) is 2305843009213693951 = 2^(64-3)-1
   ;; 最大値にしていると，eshellの起動でエラーになることがわかったので，もう少し小さい値 2^(32-3)-1 = 536870911 にする．
   ;; 大きい値にしていると，eshellの起動が遅くなることがわかったので，さらに小さい値にする :/
   '(eshell-history-size 100000)
   ;; 連続して行なった同じコマンドが重複してヒストリに登録されず，1件だけ登録されるようにする
   '(eshell-hist-ignoredups t))
  ;; eshell コマンド入力時に M-p を押すと履歴を helm でたどる
  ;; (bind-keys :map eshell-mode-hook ("M-p" . helm-eshell-history))
  ;; だと動作しなかった
  (defun eshell-mode-hook-for-helm-eshell-history ()
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  (add-hook 'eshell-mode-hook 'eshell-mode-hook-for-helm-eshell-history))

;; yasnippet
(el-get-bundle yasnippet)
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; helm
(el-get-bundle helm)
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-;" . helm-for-files)
   ("C-o" . helm-imenu)
   ("M-%" . helm-regexp)
   ("<help> a" . helm-apropos))
  :config
  (helm-migemo-mode t)
  (custom-set-variables
   ;; OSX のときは helm-locate でファイルを検索するときに Spotlight を使う
   ;; https://github.com/xiaohanyu/oh-my-emacs/issues/59
   '(helm-locate-command
     (case system-type
       ('gnu/linux "locate -i -r %s")
       ('berkeley-unix "locate -i %s")
       ('windows-nt "es %s")
       ('darwin "mdfind -name %s %s")
       (t "locate %s"))))
  (bind-keys :map helm-map
             ("C-h" . delete-backward-char))
  ;; 非同期読み込みにしていると，読み込みのタイミングによっては
  ;; 後続の処理で helm の変数を見つけられなくてエラーになることがあったので
  ;; 同期的に読み込まれるようにする
  :demand t)

;; インクリメンタルサーチを機能拡張する
(el-get-bundle ace-isearch
  :depends (avy helm-swoop ace-jump-mode))
(use-package ace-isearch
  :config
  (bind-keys :map helm-swoop-map
             ;; ace-isearch との操作感を統合する
             ((kbd "C-r") . helm-previous-line)
             ((kbd "C-s") . helm-next-line))
  (custom-set-variables
   '(ace-isearch-submode 'ace-jump-char-mode))
  (global-ace-isearch-mode 1))

;; describe-bindings の結果を helm で見られるようにする
(el-get-bundle helm-descbinds)
(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

;; yasnippet のテンプレート一覧を helm で見られるようにする
(el-get-bundle emacs-jp/helm-c-yasnippet)
(use-package helm-c-yasnippet
  :bind
  (("C-c y" . helm-yas-complete)))

;; git client
(el-get-bundle magit)
(use-package magit
  :init
  (delete 'Git vc-handled-backends)
  :bind
  (("C-M-l" . magit-status)))

;; flycheck
(el-get-bundle flycheck)
(use-package flycheck
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(el-get-bundle flycheck-pos-tip)
(use-package flycheck-pos-tip
  :config
  (custom-set-variables
   '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

;; projectile
(el-get-bundle projectile)
(use-package projectile
  :bind
  (("M-g M-r" . projectile-grep)))

;; 補完機能を利用する
(el-get-bundle company-mode)
(use-package company-mode
  :init
  (global-company-mode 1)
  (bind-keys :map company-active-map
             ;; C-sで絞り込む
             ("C-s" . company-filter-candidates)
             ;; C-hはヘルプではなく削除に割り当てる
             ("C-h" . nil)
             ;; 1つしか候補がなかったらtabで補完，複数候補があれば次の候補を選択
             ("<tab>" . company-complete-common-or-cycle))
  (custom-set-variables
   ;; 補完を1文字目から始める
   '(company-minimum-prefix-length 1)))

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
   '(twittering-use-native-retweet t)
   ;; 元投稿の Reply 先や ハッシュタグを引き継ぐ
   ;; "@hoge @fuga #foo aaa" の投稿にリプライすると
   ;; "@hoge @fuga #foo" の状態から書きはじめられる
   '(twittering-edit-skeleton 'inherit-any))
  ;; twitter client 名を変える
  (setq twittering-oauth-consumer-key (base64-decode-string "Q2tuVklCTUxVRHdIN01BOXg0V0huZw=="))
  (setq twittering-oauth-consumer-secret (base64-decode-string "NEpWVWhTVk4zVGJSeXZOUnZuakJ3YlpqdUF0RUV6UzhqWHpGWlhma1U=")))

;;; Firefox クライアント
;; Firefox に Add-on を入れておくと Firefox が telnet サーバーを立ててくれる
;; https://addons.mozilla.org/En-us/firefox/addon/mozrepl/
;; そのサーバーに繋いで Emacs から Firefox を操作したり，Firefox の各種情報を取得するためのクライアント
;;
;; 参考 http://blogs.openaether.org/?p=236
(el-get-bundle moz-repl)
(use-package moz
  :config
  ;; http://blogs.openaether.org/?p=236
  (defun jk/moz-get (attr)
    (comint-send-string (inferior-moz-process) attr)
    ;; try to give the repl a chance to respond
    (sleep-for 0 100))
  (defun jk/moz-get-current-url ()
    (interactive)
    (jk/moz-get "repl._workContext.content.location.href"))
  (defun jk/moz-get-current-title ()
    (interactive)
    (jk/moz-get "repl._workContext.content.document.title"))
  (defun jk/moz-get-current (moz-fun)
    (funcall moz-fun)
    ;; doesn't work if repl takes too long to output string
    (save-excursion
      (set-buffer (process-buffer (inferior-moz-process)))
      (goto-char (point-max))
      (previous-line)
      (setq jk/moz-current (buffer-substring-no-properties
                            (+ (point-at-bol) (length moz-repl-name) 3)
                            (- (point-at-eol) 1))))
    (message "%s" jk/moz-current)
    jk/moz-current)
  (defun jk/moz-url ()
    (interactive)
    (jk/moz-get-current 'jk/moz-get-current-url))
  (defun jk/moz-title ()
    (interactive)
    (jk/moz-get-current 'jk/moz-get-current-title)))

;;; logalimacs
(el-get-bundle logalimacs)
(use-package logalimacs
  :bind
  (("M-g M-a" . loga-add)
   ("C-t" . loga-lookup-in-popup)))

;;; epc
;; https://github.com/kiwanami/emacs-epc
;; Emacsで他の言語とRPC(ja) http://qiita.com/kiwanami/items/939fab6ac6588d72edbd
(el-get-bundle epc)
(use-package epc)

;;; restclient (EmacsからHTTPリクエストを発行，結果を見る)
;; https://github.com/pashky/restclient.el
(el-get-bundle restclient)

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

;;; Org

(el-get-bundle org-mode)
(use-package org
  :mode
  ;; org-default-notes-file のデフォルト値は .notes になっている
  (("\\.notes\\'" . org-mode))
  :bind
  (("C-c c" . org-capture))
  :init
  (defun org-mode-hooks ()
    (skk-mode 1)
    (org-display-inline-images))
  (add-hook 'org-mode-hook 'org-mode-hooks)
  :config
  (custom-set-variables
   ;; orgファイルを開いたときに畳んだ状態で表示しない（全て表示する）
   '(org-startup-folded nil)
   ;; orgファイルの中で画像をインライン表示する
   '(org-startup-with-inline-images t)
   ;; src ブロックの中を色付けする
   '(org-src-fontify-natively t)
   ;; plantumlで利用する
   '(org-plantuml-jar-path "/usr/local/Cellar/plantuml/8048/plantuml.8048.jar")))

;; org-babel
(el-get-bundle victorolinasc/ob-elixir) ; org-babel で Elixir を扱う
(el-get-bundle alf/ob-restclient.el) ; org-babel で restclient を扱う

(use-package ob
  :init
  ;; org-babelで使う言語を設定する
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (elixir . t)
     (restclient . t)
     (plantuml . t)))
  (defun org-babel-after-execute-hooks ()
    (org-display-inline-images))
  (add-hook 'org-babel-after-execute-hook 'org-babel-after-execute-hooks)
  :config
  ;; org-babel tangle でソースコードを書き出す時に先頭の1行をあけない
  (add-to-list 'org-babel-default-header-args '(:padline . "no"))
  (custom-set-variables
   ;; コードを評価するときに尋ねない
   '(org-confirm-babel-evaluate nil)))

;;; PlantUML
(el-get-bundle skuro/plantuml-mode)
(use-package plantuml-mode
  :config
  (custom-set-variables
   '(plantuml-output-type "png") ;; FIXME pngだと期待通りの画像が表示されない
   '(plantuml-jar-path "/usr/local/Cellar/plantuml/8048/plantuml.8048.jar")))

;;; Nikulog

(setq nikulog-path "~/nikulog")

(defun nikulog--get-year-month-day ()
  "Return (year month day) ex. (\"2015\" \"05\" \"08\")"
  (-let (((_ &as sec minute hour day month year dow dst zone) (decode-time)))
    (->> (list year month day)
         (-map (lambda(x) (format "%02d" x))))))

(defun nikulog--create-directory-and-return-path (path year month day)
  "Create directory if not exists, and then return path"
  (let ((dir (list path year month day)))
    (apply 'f-mkdir dir)
    (apply 'f-join dir)))

(defun nikulog-path-today ()
  (let ((path-today (cons nikulog-path (nikulog--get-year-month-day))))
    (apply 'nikulog--create-directory-and-return-path path-today)
    path-today))

(defun nikulog-title-today ()
  (s-join "-" (nikulog--get-year-month-day)))

(defun nikulog-add-internal-link-to-region (&optional beg end)
  "Add url encoded internal link (like \"ほげ\" -> \"#%E3%81%BB%E3%81%92\") to region"
  (interactive "r")
  (if (use-region-p)
      (let ((str (buffer-substring-no-properties beg end)))
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert (format "[[#%s][%s]]" (url-encode-url str) str))))))

(with-eval-after-load 'org-capture
  (let* ((filepath (apply 'f-join (append (nikulog-path-today) '("index.org"))))
         (title (nikulog-title-today)))
    ;; http://stackoverflow.com/questions/8614642/how-to-type-a-dynamic-file-entry-for-org-capture
    ;; バッククォートの中では "," をつけたものだけ評価/展開される
    ;; http://qiita.com/snmsts@github/items/ef625bd6be7e685843ca
    (add-to-list 'org-capture-templates `("f" "Firefox to Nikulog" entry (file+headline ,filepath ,title) "* %?\n%(concat \"[[\" (jk/moz-url) \"][\" (jk/moz-title) \"]]\")\n\n"))
    (add-to-list 'org-capture-templates `("l" "Entry to Nikulog" entry (file+headline ,filepath ,title) "* %?"))))

;;; Markdown
(el-get-bundle markdown-mode)
(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode))
  :commands
  (markdown-mode))

;;; SQL
(el-get-bundle sqlup-mode)
(use-package sql
  :init
  ;; 読み込み後(config:)に設定するとうまく動かない
  (custom-set-variables
   ;; 履歴を保存する
   '(sql-input-ring-file-name "~/.sql-input-ring")
   '(sql-product 'postgres))

  (defun sql-mode-hooks ()
    (sqlup-mode))

  (defun sql-interactive-mode-hooks ()
    ;; 横方向に長い場合に折り返さない
    (toggle-truncate-lines t)
    (sqlup-mode))
  (add-hook 'sql-mode-hook 'sql-mode-hooks)
  (add-hook 'sql-interactive-mode-hook 'sql-interactive-mode-hooks))

;; sqlファイルのインデントを整える
(el-get-bundle emacswiki:sql-complete)
(el-get-bundle emacswiki:sql-indent
  :depends
  (sql-complete))
(use-package sql-indent)

;;; Yaml
(el-get-bundle yaml-mode)
(use-package yaml-mode)

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

;; RSpec
(el-get-bundle rspec-mode)
(use-package rspec-mode
  :config
  (with-eval-after-load 'rspec-mode
    '(rspec-install-snippets))
  (bind-keys :map rspec-mode
             ("C-9" . rspec-toggle-spec-and-target)))

;;; Web
(el-get-bundle web-mode)
(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode))
  :config
  (custom-set-variables
   '(web-mode-markup-indent-offset 2)))

;;; JavaScript
(use-package js-mode
  :mode
  (("\\.jshintrc\\'" . js-mode))
  :config
  (custom-set-variables
   '(js-indent-level 2)))

;;; JSON
(use-package json
  :init
  (custom-set-variables
   '(js-indent-level 2)))

;;; TypeScript
(el-get-bundle typescript-mode)
(el-get-bundle tide)
(use-package tide
  :config
  (defun typescript-mode-hooks-for-tide ()
    (tide-setup)
    (flycheck-mode t)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode t)
    (company-mode-on))
  (add-hook 'typescript-mode-hook 'typescript-mode-hooks-for-tide))

;;; Erlang
(el-get-bundle erlang)
(use-package erlang
  :config
  (require 'erlang-start))

;;; Elixir
(el-get-bundle elixir)
(use-package elixir-mode)

;; Elixir の mix や iex などを Emacs から使えるようにしてくれる
(el-get-bundle alchemist)
(use-package alchemist
  :config
  (custom-set-variables
   ; https://github.com/tonini/alchemist.el/issues/71
   '(alchemist-goto-elixir-source-dir "~/src/elixir")
   '(alchemist-goto-erlang-source-dir "~/src/otp"))
  ; https://github.com/tonini/alchemist.el#definition-lookup
  (defun custom-erlang-mode-hook ()
    (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
  (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
  (bind-keys :map alchemist-mode-map
             ("C-9" . alchemist-project-toggle-file-and-tests)))

;;; Clojure
(el-get-bundle clojure-mode)
(use-package clojure-mode)

;;; Rust
(el-get-bundle rust-mode)
(el-get-bundle flycheck-rust)
(use-package flycheck-rust
  :config
  (defun flycheck-mode-hooks-for-flycheck-rust ()
    (flycheck-rust-setup))
  (add-hook 'flycheck-mode-hook 'flycheck-mode-hooks-for-flycheck-rust))

(el-get-bundle emacs-racer)
(use-package racer-mode
  :init
  (defun racer-mode-hooks-for-utils ()
    (eldoc-mode)
    (company-mode))
  (add-hook 'racer-mode-hook 'racer-mode-hooks-for-utils)
  (defun rust-mode-hooks-for-racer-mode ()
    (racer-mode))
  (add-hook 'rust-mode-hook 'rust-mode-hooks-for-racer-mode)
  (custom-set-variables
   '(racer-rust-src-path "~/src/rust/src")))

;;;
;;; ローカルな環境で利用するようなelisp
;;;
(load "~/local-elisp.el" t)
(put 'downcase-region 'disabled nil)
