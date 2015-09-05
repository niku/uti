#!/usr/bin/env bash
#
# OSX 環境構築用スクリプト
#

# Homebrew をインストールするまで
# https://github.com/Homebrew/homebrew/blob/master/share/doc/homebrew/Installation.md#installation
#
# 1.
# XCODE をインストール
# https://itunes.apple.com/us/app/xcode/id497799835
#
# 2.
# XCODE のライセンスに同意
# $ xcodebuild -license
#
# 3.
# Command Line Tools for Xcode をインストール
# $ xcode-select --install
#
# 4.
# Homebrew をインストール
# $ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
#

# Homebrew Cask でのアプリのインストール先を ~/Applications から /Applications へと変更する
# https://github.com/caskroom/homebrew-cask/blob/master/USAGE.md#options
HOMEBREW_CASK_OPTS="--appdir=/Applications"

# このリポジトリのルートパスを設定する
# http://qiita.com/yudoufu/items/48cb6fb71e5b498b2532
# http://freak-da.hatenablog.com/entry/20121023/p1
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE:-${(%):-%N}}")"; pwd)" # このファイルの場所
ROOT_DIR=`cd -P "${SCRIPT_DIR}/../.."; pwd`

brew update

brew tap homebrew/binary
brew tap homebrew/versions
brew tap caskroom/homebrew-cask
brew tap aereal/aereal_casks
brew tap sanemat/font

brew install brew-cask
brew install coreutils
brew install texinfo
brew link texinfo --force
brew install zsh
brew install git
brew install cmake
brew install tmux
brew install mosh
brew install gnupg
brew install ricty
brew install emacs --with-cocoa --with-librsvg
brew link emacs
brew linkapps emacs --local
brew install cask
brew install cmigemo
brew link cmigemo
brew install curl
brew install packer
brew install readline
brew install rbenv
brew install rbenv-default-gems
brew install ruby-build
brew install postgres
brew install redis
brew install the_silver_searcher
brew install jq
brew install tree
brew install node
brew install npm
npm install -g bower
npm install -g eslint
npm install -g jsonlint
npm install -g jsdoc
npm install -g source-map-support
npm install -g grunt-cli
npm install -g typescript-tools
brew install erlang
brew install elixir
brew install go
brew install graphviz
brew install hub
brew install mplayer
brew install heroku-toolbelt
brew install awscli
brew install ffmpeg
brew install imagemagick
brew install groonga
brew install maven # for embulk test
brew install ant
brew install android-sdk
brew install android-ndk
brew install closure-compiler
brew install clojurescript
brew install leiningen
brew install rlwrap
brew install poppler
brew cask install java
brew cask install karabiner
brew cask install aquaskk
brew cask install virtualbox
brew cask install vagrant
vagrant plugin install vagrant-omnibus
vagrant plugin install vagrant-aws
vagrant plugin install vagrant-itamae
brew cask install td-agent
brew cask install dash
brew cask install dropbox
brew cask install evernote
brew cask install skitch
brew cask install onepassword
brew cask install firefox
brew cask install google-chrome
brew cask install libreoffice
brew cask install alfred
brew cask install google-drive
brew cask install google-hangouts
brew cask install genymotion
brew cask install xquartz
brew cask install kindle

brew cleanup
brew cask cleanup

ln -s ${ROOT_DIR}/.emacs.d ~/
ln -s ${ROOT_DIR}/share ~/

# shell history for erl / iex
# https://github.com/ferd/erlang-history
cd /var/tmp && \
    git clone ferd/erlang-history && \
    cd erlang-history && \
    sudo make install
