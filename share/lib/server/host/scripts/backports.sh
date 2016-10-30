#!/bin/bash -eux

cat << EOS > /etc/apt/sources.list.d/jessie-backports.list
deb http://ftp.debian.org/debian jessie-backports main
EOS
