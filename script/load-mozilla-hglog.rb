#!/usr/bin/env ruby

require 'yaml'
require 'fileutils'

config = YAML.load_file('../config/mozilla.yml')
source_path = File.expand_path(config["source"]["path"])
output_path = File.absolute_path('../data/mozilla-hg-log.bz2')
pwd = FileUtils.pwd

FileUtils.chdir(source_path)
puts "cd #{source_path}"

cmd = %q{hg log --template "{node|short} | {date|isodate} | {author} | {desc|firstline}\n" --date "2009-01-01 to 2013-12-31" | bzip2 > } + output_path
puts cmd
system cmd 

FileUtils.chdir(pwd)

# hint for gen-makefile:
# File.open("../data/mozilla-hg-log.bz2", "w") { |f| **whatever** }
