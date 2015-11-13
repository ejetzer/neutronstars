#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys, subprocess, os, glob, os.path

def make_movies(path, src='grid1', fmt='mp4'):
    # Output file name
    out = src + '.' + fmt
    out = os.path.join(path, fmt, out)
    # Check that path/fmt exists
    if not os.path.isdir(os.path.join(path, fmt)):
        os.makedirs(os.path.join(path, fmt))
    # Input file model
    src = src + '_*.png'
    src = os.path.join(path, 'png', src)
    yield 'Taking files from', src
    yield 'Making movie', out
    # Go into path
    currdir = os.getcwd()
    os.chdir(path)
    yield 'Switched from', currdir, 'to', path
    # Make movies
    cmd = 'images_to_movie.sh \'{}\' \'{}\''.format(src, out)
    yield (cmd,)
    code = subprocess.call(cmd, shell=True)
    yield ('Done making movie.',)
    # Go back to last path
    os.chdir(currdir)
    yield 'Back to', currdir

if __name__ == '__main__':
    path, src, fmt = '.', 'grid1', 'mp4'
    if '--path' in sys.argv:
        index = sys.argv.index('--path') + 1
        path = sys.argv[index]
    if '--src' in sys.argv:
        index = sys.argv.index('--src') + 1
        src = sys.argv[index]
    if '--fmt' in sys.argv:
        index = sys.argv.index('--fmt') + 1
        fmt = sys.argv[index]
    paths = glob.glob(path)
    print(paths)
    for path in paths:
        print('Making movie for', path)
        for message in make_movies(path, src, fmt):
            print(*message)
        print('Movie made for', path)
