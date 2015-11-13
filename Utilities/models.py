#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Generate variations on a MESA model.

@author: ejetzer
'''

import matplotlib.pylab as plt, spinmob, subprocess, os, glob,\
       sys, shutil
import rn

MODEL = 'Model/'
OUT = 'Rate {:02.2e}/'

def make_models(start=2e-8, end=2e-11, N=15, model=MODEL, out=OUT,
                run=False, copy=True, **kargs):
    start, end = plt.log(start), plt.log(end)
    yield 'Will generate models with `mass_change` from {} to {}'.format(start, end)
    accretion_rates = plt.exp(plt.linspace(start, end, N))
    yield '`mass_change` in {}'.format(accretion_rates)
    yield 'Reading model from \'{}\''.format(model)
    with open(model+'inlist_ns_h', 'r') as model_file:
        model_cntnt = model_file.read()
    for rate in accretion_rates:
        if copy:
            yield 'Copying to {}'.format(out.format(rate))
            shutil.copytree(model, out.format(rate))
        yield 'Writing to \'{}\''.format(out.format(rate)+'inlist_ns_h')
        with open(out.format(rate)+'inlist_ns_h', 'w') as inlist:
            inlist.write(model_cntnt.format(rate))
        if run:
            iterator = rn.rn(path=out.format(rate), **kargs)
            try:
                for message in iterator: yield message
            except KeyboardInterrupt:
                print 'Interrupted by user. Moving on!'
            

if __name__ == '__main__':
    path, run, make, movie, out, copy, re = MODEL, True, True, False, OUT, True, 10
    if '--path' in sys.argv:
        index = sys.argv.index('--path') + 1
        path = sys.argv[index]
    if '--output' in sys.argv:
        index = sys.argv.index('--output') + 1
        out = sys.argv[index]
    if '--re' in sys.argv:
        index = sys.argv.index('--re') + 1
        re = int(sys.argv[index])
    if '--no-cp' in sys.argv: copy = False
    if '--no-rn' in sys.argv: run = False
    if '--no-mk' in sys.argv: make = False
    if '--movie' in sys.argv: movie = True
    for message in make_models(model=path, out=out, copy=copy,
                               make=make, run=run, movie=movie,
                               re=re):
        print message
