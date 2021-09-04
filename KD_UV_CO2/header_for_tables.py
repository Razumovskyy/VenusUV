#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 14 15:46:41 2021

@author: taskerbliss
"""

import numpy as np

fmt = '%.1f', '%1.2e'
uv_dir = './UV_absorption/'
uv_files = ['H2O/H2O_JPL2011.dat', 'H2O/H2O_Watanabe1953.dat', 'CO2/CO2_Parkinson2003.dat', 
            'CO2/CO2_Shemansky1972.dat', 'CO2/CO2_Thompson1963.dat', 'OCS/OCS_Molina1981.dat', 
            'SO2/SO2_ManattLane1993.dat', 'HCl/HCl_Inn1975.dat']

for p in uv_files:
    
    if p == 'CO2/CO2_Parkinson2003.dat':
        fmt = '%.5f', '%1.5e'
    elif p == 'SO2/SO2_ManattLane1993.dat':
        fmt = '%.1f', '%1.3e'
    else:
            fmt = '%.2f', '%1.2e' 
        
    f = uv_dir + p
    f1 = open (f)
    header = f1.readline()
    wave_len, cross_sec = np.loadtxt(f, comments='#', unpack=True)
    leng = len (wave_len)
    header = header + str(leng)
    np.savetxt(f, np.transpose((wave_len, cross_sec)), header=header, delimiter='  ', comments='', 
               fmt=fmt)

    
    