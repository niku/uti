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
  (setq ns-alternate-modifier (quote super)))

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
