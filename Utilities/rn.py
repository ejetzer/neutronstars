#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on Sun Nov  8 15:05:58 2015

@author: ejetzer
"""
from __future__ import print_function

import subprocess, os, glob, sys
import make_all_movies
import update_inlist

def rn(path='.', make=True, run=True, movie=True, re=0):
    'Makes & runs a MESA model, and transforms the produced images \
into a movie.'
    currdir = os.getcwd()
    os.chdir(path)
    yield 'Moved from \'{}\' to \'{}\'.'.format(currdir, path)
    if make:
        yield 'Making...'
        code = subprocess.call('./mk')
        yield 'Made (exited with status {})!'.format(code)
    if run:
        # Remove the pictures from the last run...
        imgs = glob.glob('png/*')
        for img in imgs: os.remove(img)
        yield 'Runnning...'
        update_inlist.replace('do_element_diffusion = .true.',
                              'do_element_diffusion = .false.')
        update_inlist.replace('steps_to_take_before_terminate = -1',
                    'steps_to_take_before_terminate = {}'.format(re))
        try:
            code = subprocess.call('./rn', shell=True)
        except KeyboardInterrupt:
            yield 'Exited ./rn'
        cmd = './re x{:03d}'.format(re)
        update_inlist.replace('do_element_diffusion = .false.',
                              'do_element_diffusion = .true.')
        update_inlist.replace(
                    'steps_to_take_before_terminate = {}'.format(re),
                    'steps_to_take_before_terminate = -1')
        yield 'Restarting with photo x{:03d}'.format(re)
        try:
            code = subprocess.call(cmd, shell=True)
        except KeyboardInterrupt:
            yield 'Exited ./re'
        yield 'Ran (exited with status {})!'.format(code)
        update_inlist.replace('do_element_diffusion = .true.',
                              'do_element_diffusion = .false.')
        update_inlist.replace('steps_to_take_before_terminate = -1',
                    'steps_to_take_before_terminate = {}'.format(re))
    if movie:
        yield 'Making movie...'
        make_all_movies.make_movies('.', fmt='mp4')
        make_all_movies.make_movies('.', fmt='gif')
        yield 'Movie made (exited with status {})!'.format(code)
    yield 'Moving back to \'{}\'.'.format(currdir)
    os.chdir(currdir)
    
if __name__ == '__main__':
    path, run, make, movie, re = os.getcwd(), True, True, False, 0
    if '--path' in sys.argv:
        index = sys.argv.index('--path') + 1
        path = sys.argv[index]
    if '--re' in sys.argv:
        index = sys.argv.index('--re') + 1
        re = int(sys.argv[index])
        print('Will restart at x{:03d}'.format(re))
    if '--no-rn' in sys.argv: run = False
    if '--no-mk' in sys.argv: make = False
    if '--movie' in sys.argv: movie = True
    for message in rn(path, make, run, movie, re):
        print(message)
