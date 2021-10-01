#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep  7 15:05:44 2021

@author: taskerbliss
"""

import matplotlib.pyplot as plt
import numpy as np


heightm, nmols, crsec, fdo = np.loadtxt("./RUN_KD_DO/MOLS_SIGMA_FDO_2ANGLE.dat", skiprows=1 , unpack=True)
height1, nmols1, crsec1, fdo1 = np.loadtxt("/home/taskerbliss/Documents/Science/МИХАИЛ/8K_FDO_.75deg", unpack=True)

fig, ax = plt.subplots()

ax.loglog(nmols, crsec, color='red')
ax.loglog(nmols1, crsec1, color = 'blue')

ax.set_xlim(1e26, 1e14)

ax.set_xlabel('number of molecules along solar ray [cm^-2]')
ax.set_ylabel('KD cross sec, cm^2')

fig.suptitle('K-term for SO2 FUV. Zenith angle is 75 deg')

ax.grid(True)

plt.show()


# =============================================================================
# z, nmols, crsec, FDO  = np.loadtxt("K_FDO_.00deg", skiprows=0 , unpack=True)
# 
# fig, ax = plt.subplots()
# 
# ax.semilogy(z, crsec)
# 
# #ax.set_xlim(1e26, 1e14)
# 
# #ax.set_xlabel('number of molecules along solar ray [cm^-2]')
# ax.set_xlabel('height [km]')
# ax.set_ylabel('KD cross sec, cm^2')
# ax.set_title('K-term for CO2 FUV. Zenith angle is 0 deg')
# ax.grid(True)
# 
# plt.show()
# =============================================================================
