#!/bin/bash

apt-get install -y build-essential module-assistant
echo 'yes' |  m-a prepare
mkdir /tmp/vbox
mount -o loop /home/vagrant/VBoxGuestAdditions.iso /tmp/vbox
sh /tmp/vbox/VBoxLinuxAdditions.run
umount /tmp/vbox
rmdir /tmp/vbox
rm /home/vagrant/*.iso
