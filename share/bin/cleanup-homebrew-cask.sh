#!/usr/bin/env ruby

require "fileutils"

casks = Dir.glob "/opt/homebrew-cask/Caskroom/*"
casks.each do |cask|
  versions = Dir.glob(File.join(cask, "*"))
  latest_version = versions.max_by { |v| File.ctime(v) }

  old_versions = versions - [latest_version]
  old_versions.each do |old_version|
    FileUtils.rm_rf(old_version)
  end
end
