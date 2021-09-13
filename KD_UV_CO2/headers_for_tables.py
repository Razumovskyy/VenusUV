#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 14 15:46:41 2021

@author: taskerbliss
"""

import numpy as np

fmt = '%.2f', '%1.2e'
uv_dir = './UV_absorption/CO2/'
uv_files = ['CO2_Bastien1985.dat', 'CO2_Inn1953.dat', 'CO2_Kuo2004.dat', 
            'CO2_LewisCarver1983.dat', 'CO2_Rabalais1971.dat', 'CO2_Watanabe1953.dat', 
            'CO2_Yoshino1996.dat', 'CO2_aggregate.dat']

for p in uv_files:
    
    p = 'CO2_aggregate.dat'
    if p == 'CO2_aggregate.dat':
        fmt = '%.4f', '%1.4e'
            
    f = uv_dir + p
    f1 = open (f)
    header = f1.readline()
    wave_len, cross_sec = np.loadtxt(f, comments='#', unpack=True)
    leng = len (wave_len)
    header = header + str(leng)
    np.savetxt(f, np.transpose((wave_len, cross_sec)), header=header, delimiter='  ', comments='', 
               fmt=fmt)
    
    break

    
    