#!/usr/bin/env bash
#
# OSX 環境構築用スクリプト
#

############
# 手動処理 #
############

# XCode をインストールする
#
# 1.
# Command Line Tools for Xcode をインストール．
# その際に Xcode 本体もインストールできる．
# $ sudo xcode-select --install
#
# 1-2.
# (もしインストールできなければ) Xcode をインストール
# https://itunes.apple.com/us/app/xcode/id497799835
#
# 2.
# XCODE のライセンスに同意
# $ sudo xcodebuild -license

# Homebrew をインストールする
# https://github.com/Homebrew/homebrew/blob/master/share/doc/homebrew/Installation.md#installation
#
# $ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# AquaSKK をインストールする
# https://github.com/codefirst/aquaskk
#
# https://github.com/codefirst/aquaskk/releases/download/4.2.6/AquaSKK-4.2.6.dmg

# iTerm2 をインストールする
# http://mzp.hatenablog.com/entry/2015/03/15/213219
#
# https://github.com/mzp/iTerm2/releases/download/AquaSKK/iTerm2.zip

# 1Password(4.x) をインストールする
# https://agilebits.com/downloads
#
# https://d13itkw33a7sus.cloudfront.net/dist/1P/mac4/1Password-4.4.3.zip

############
# 自動処理 #
############

# Homebrew Cask でのアプリのインストール先を ~/Applications から /Applications へと変更する
# https://github.com/caskroom/homebrew-cask/blob/master/USAGE.md#options
HOMEBREW_CASK_OPTS="--appdir=/Applications"

brew update

brew install caskroom/cask/brew-cask
brew cask install xquartz
brew cask install java
brew cask install karabiner
brew cask install virtualbox
brew cask install vagrant
brew cask install dash
brew cask install dropbox
brew cask install evernote
brew cask install skitch
brew cask install firefox
brew cask install google-chrome
brew cask install libreoffice
brew cask install alfred
brew cask install google-drive
brew cask install google-hangouts
brew cask install genymotion
brew cask install kindle
brew cask install skype
brew cask install gfxcardstatus
brew cask cleanup

brew tap homebrew/binary
brew tap homebrew/versions
brew tap homebrew/devel-only
brew tap sanemat/font
brew install coreutils
brew install texinfo
brew link texinfo --force
brew install zsh
brew install git
brew install cmake
brew install tmux
brew install gnupg
brew install ricty && \
    cp -f /usr/local/Cellar/ricty/*/share/fonts/Ricty*.ttf ~/Library/Fonts/ && \
    fc-cache -vf
brew install ssh-copy-id
brew install emacs --with-cocoa --with-librsvg
brew link emacs
brew linkapps emacs
brew install cask
brew install cmigemo
brew link cmigemo
brew install curl
brew install packer
brew install docker-compose
brew install readline
brew install rbenv
brew install ruby-build --HEAD
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
npm install -g typescript
npm install -g typescript-tools
brew install erlang
brew install --devel homebrew/devel-only/rebar3
brew install elixir
brew install go
brew install rust
cargo install racer
brew install graphviz
brew install hub
brew install gist
brew install mpv
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

brew cleanup

# shell history for erl / iex
# https://github.com/ferd/erlang-history
cd /var/tmp && \
    git clone https://github.com/ferd/erlang-history.git && \
    cd erlang-history && \
    sudo make install

# Mac のディレクトリを英語にする
# http://qiita.com/is0me/items/0b7b846f1f0860629950
cd /System/Library/CoreServices/SystemFolderLocalizations/ja.lproj && \
    sudo mv SystemFolderLocalizations.strings SystemFolderLocalizations.strings.back && \
    sudo cp ../en.lproj/SystemFolderLocalizations.strings .
