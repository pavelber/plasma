#!/usr/bin/python
import sys
import os

def convert(in_file, out_file):
	if os.path.exists(in_file):
		with open(in_file, 'rb') as inf:
			with open(out_file, 'wb') as outf:
			# process
	else
		sys.stderr.write('Can\'t open transitions file ' + in_file + '\n')
		exit(1)

if len(sys.argv)!=3:
	sys.stderr.write('Usage: ' + sys.argv[0] + 'directory-with-cFAC-1.6.3-files  output-directory\n')
	exit(1)


in_dir = sys.argv[1]
out_dir = sys.argv[2]



if os.path.exists(sys.argv[1]+"/fac.ai"):
	with open(sys.argv[1], 'rb') as in_ai:
		# process
else
	sys.stderr.write('Can\'t open transitions file ' + sys.argv[1]+"/fac.ai"+'\n')
	exit(1)

