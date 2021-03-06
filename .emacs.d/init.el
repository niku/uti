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
  (if (f-readable? custom-set-variable-file)
      (load custom-set-variable-file)
    (f-touch custom-set-variable-file)
    (load custom-set-variable-file)))

(setq inhibit-startup-screen t)         ; startupの画面を消す
(setq initial-scratch-message nil)      ; *scratch*のメッセージを消す
(menu-bar-mode 0)                       ; メニューバーを消す
(tool-bar-mode 0)                       ; ツールバーを消す
(set-scroll-bar-mode nil)               ; スクロールバーを消す
(setq visible-bell t)                   ; ビープ音を消す
(blink-cursor-mode 0)                   ; カーソルを点滅させない
(global-hl-line-mode 1)                 ; 現在行に色をつける
(setq column-number-mode t)             ; 列番号をモードラインに表示する
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

(use-package epg
  :config
  (custom-set-variables
   '(epg-gpg-program "gpg1")))

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
   '(eishell-hist-ignoredups t)))

;; yasnippet
(el-get-bundle yasnippet)
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; ivy/counsel
(el-get-bundle counsel)
(use-package counsel
  :init
  (counsel-mode 1)
  :config
  (ivy-mode 1)
  (defun niku-toggle-projectile-lists ()
    "Returns list of current project files if It is in a project, else list of known project files."
    (cond ((projectile-project-p)
           (projectile-current-project-files))
          (t
           projectile-known-projects)))
  ;; TODO
  ;; listを設定するところまではできたが，開くアクションをしても空のバッファを開くことになる．
  ;; バッファがあれば，そのバッファ，なければファイルを開くようなアクションを設定しなければいけないかも．
  ;;
  ;; (ivy-set-sources
  ;;  'ivy-switch-buffer
  ;;  '((original-source)
  ;;    (niku-toggle-projectile-lists)))

  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :bind
  ("C-s" . swiper)
  ("C-o" . counsel-imenu)
  ("C-;" . ivy-switch-buffer)
  ("<f6>" . ivy-resume)
  ("<help> a" . counsel-apropos))

;; git client
(el-get-bundle magit)
(use-package magit
  :init
  (delete 'Git vc-handled-backends)
  :bind
  (("C-M-l" . magit-status)))

;; flycheck
(el-get-bundle flycheck)
; http://www.flycheck.org/en/latest/user/installation.html#use-package
(use-package flycheck
  :init (global-flycheck-mode))

(el-get-bundle flycheck-pos-tip)
(use-package flycheck-pos-tip
  :config
  (custom-set-variables
   '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

;;; インデントを強調表示する
;; (browse-url "https://github.com/antonj/Highlight-Indentation-for-Emacs")
(el-get-bundle highlight-indentation)
(use-package highlight-indentation
  :config
  (add-hook 'prog-mode-hook #'highlight-indentation-mode)
  (add-hook 'yaml-mode-hook #'highlight-indentation-mode))

;;; projectile
(el-get-bundle projectile)
(use-package projectile
  :init
  (projectile-mode t)
  :custom
  (projectile-completion-system 'ivy)
  (compilation-read-command nil)           ; コマンド実行時にプロンプトを表示しない
  (projectile-create-missing-test-files t) ; テストファイルがなければ作る
  :bind
  (("C-9" . projectile-toggle-between-implementation-and-test)
   ("C-0" . projectile-test-project)))

(el-get-bundle counsel-projectile)
(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

;;; compile
(use-package compile
  :init
  ;; (projectileのテスト結果の表示に使う)Compilation Mode のバッファで ANSI Color の表示を有効にする
  ;; (browse-url "https://stackoverflow.com/a/13408008")
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;; 補完機能を利用する
(el-get-bundle company-mode)
(use-package company
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
  :config
  (quickrun-add-command "python"
    '((:command . "python3"))
    :override t)
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
(use-package org-mode
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
  :custom
  ;; orgモードで画像の大きさを変えられるようにする
  (org-image-actual-width nil)
  ;; orgファイルを開いたときに畳んだ状態で表示しない（全て表示する）
  (org-startup-folded nil)
  ;; orgファイルの中で画像をインライン表示する
  (org-startup-with-inline-images t)
  ;; src ブロックの中を色付けする
  (org-src-fontify-natively t))

;; org-babel
(el-get-bundle ob-elixir) ; org-babel で Elixir を扱う
(el-get-bundle ob-restclient) ; org-babel で restclient を扱う

(use-package ob
  :init
  ;; org-babelで使う言語を設定する
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (elixir . t)
     (restclient . t)
     (plantuml . t)
     (shell . t)
     (sql . t )))
  (defun org-babel-after-execute-hooks ()
    (org-display-inline-images))
  (add-hook 'org-babel-after-execute-hook 'org-babel-after-execute-hooks)
  :config
  ;; org-babel tangle でソースコードを書き出す時に先頭の1行をあけない
  (add-to-list 'org-babel-default-header-args '(:padline . "no"))
  (custom-set-variables
   ;; コードを評価するときに尋ねない
   '(org-confirm-babel-evaluate nil)))

;; org-reveal : org-mode でスライドを書く
;; (browse-url "https://github.com/yjwen/org-reveal")
(el-get-bundle htmlize) ; org-mode でスライドを書いたときにソースコードのハイライトに必要
(use-package htmlize)
(el-get-bundle niku/org-reveal :branch "slide-id-editable")
(use-package ox-reveal
  :custom
  (org-reveal-root (expand-file-name "~/src/reveal.js"))
  (org-reveal-title-slide nil)          ; 自動生成したタイトルを使わない
  (org-reveal-single-file t)            ; JavaScriptや画像を埋め込んでオフラインでもHTML単体で動くようにする
  (org-reveal-hlevel 2)                 ; ページの切り替えを ** と *** で行う
  (org-reveal-history t)                ; スライドのページに直接遷移できるURLを生成する
  (org-reveal-slide-id-format "%s")
  )

;;; PlantUML
(el-get-bundle skuro/plantuml-mode)
(use-package plantuml-mode
  :config
  (custom-set-variables
   '(plantuml-output-type "png"))) ;; FIXME pngだと期待通りの画像が表示されない

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
    (add-to-list 'org-capture-templates `("l" "Entry to Nikulog" entry (file+headline ,filepath ,title) "* %?"))
    (let-alist (marionette-alist)
      (add-to-list 'org-capture-templates `("f" "Firefox to Nikulog" entry (file+headline ,filepath ,title) "* %?\n%(concat \"[[\" .url \"][\" .title \"]]\")\n\n"))
      .title)))

;;; Markdown
(el-get-bundle markdown-mode)
(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode))
  :commands
  (markdown-mode))

;;; Mustache
(el-get-bundle mustache-mode)
(use-package mustache-mode)

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

;;; Terraform
(el-get-bundle terraform-mode)
(use-package terraform-mode)

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
    '(rspec-install-snippets)))

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
  (custom-set-variables
   '(typescript-indent-level 2))
  (defun typescript-mode-hooks-for-tide ()
    (tide-setup)
    (flycheck-mode t)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode t)
    (company-mode-on))
  (add-hook 'typescript-mode-hook 'typescript-mode-hooks-for-tide))

;;; Vue
(el-get-bundle AdamNiederer/vue-html-mode)
(el-get-bundle ssass-mode)
(el-get-bundle edit-indirect)
(el-get-bundle vue-mode)
(use-package vue-mode)

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
  (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook))

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

(el-get-bundle racer)
(use-package racer
  :init
  (defun racer-mode-hooks-for-utils ()
    (eldoc-mode)
    (company-mode))
  (add-hook 'racer-mode-hook 'racer-mode-hooks-for-utils)
  (defun rust-mode-hooks-for-racer-mode ()
    (racer-mode))
  (add-hook 'rust-mode-hook 'rust-mode-hooks-for-racer-mode))

;;; Elm
(el-get-bundle elm-mode)
(use-package elm-mode
  :config
  (custom-set-variables
   '(elm-tags-on-save t)
   '(elm-format-on-save t))
  (defun elm-mode-hooks-for-elm-mode ()
    (add-to-list 'company-backends 'company-elm))
  (add-hook 'elm-mode-hook 'elm-mode-hooks-for-elm-mode))

;;; Marionette (new Firefox client)
;; https://firefox-source-docs.mozilla.org/testing/marionette/marionette/index.html
(defcustom marionette-python-path "python2"
  "The path which is able to execute a marionette client."
  :type 'string
  :group 'marionette)

(defconst marionette-code "import json
from marionette_driver.marionette import Marionette
client = Marionette('localhost', port=2828)
client.start_session()
url = client.get_url()
title = client.title
return_object = {'url': url, 'title': title}
json = json.dumps(return_object, separators=(',',':'))
print(json)
client.delete_session()
" "The python code which is executed.")

(defvar marionette-code-path ""
  "A path of the python code which is executed.")

(defun marionette-command ()
  "The command which will execute."
  (if (f-file-p marionette-code-path)
      (concat marionette-python-path " " marionette-code-path)
    (let ((code-path (make-temp-file "marionette" nil ".py")))
      (f-append-text marionette-code 'utf-8 code-path)
      (setq marionette-code-path code-path)
      (concat marionette-python-path " " marionette-code-path))))

(defun marionette-alist ()
  "Get alist from a browser."
  (json-read-from-string (shell-command-to-string (marionette-command))))

;;; Go-lang
;; (browse-url "https://github.com/dominikh/go-mode.el")
(el-get-bundle go-mode)
(el-get-bundle company-go)
(use-package go-mode
  :bind
  ("M-." . godef-jump))

;;; To edit protocol buffer
(el-get-bundle protobuf-mode)
(use-package protobuf-mode)

;;;
;;; ローカルな環境で利用するようなelisp
;;;
(load "~/local-elisp.el" t)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
