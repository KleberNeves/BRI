#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, string

target_dir = "/home/kleber/Dropbox/Brazilian Reproducibility Initiative/Revisão Sistemática Inicial - Fulltext/Amostra WoS/PDFs Kleber"

for filename in os.listdir(target_dir):
	
	ext = filename[-4:]
	filename = filename[:-4]
	
	new_filename = string.replace(filename, "/", "_")
	new_filename = string.replace(new_filename, ".", "_")
	new_filename = string.replace(new_filename, ",", "_")
	new_filename = string.replace(new_filename, "-", "_")
	new_filename = string.replace(new_filename, ";", "_")
	new_filename = string.replace(new_filename, ":", "_")
	
	new_filename = new_filename + ext
	filename = filename + ext
	
	if filename != new_filename:
		print filename
		print new_filename
		os.rename(target_dir + "/" + filename, target_dir + "/" + new_filename)

	print "\n"