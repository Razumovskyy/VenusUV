#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep  7 15:05:44 2021

@author: taskerbliss
"""

import matplotlib.pyplot as plt
import numpy as np


nmols, crsec = np.loadtxt("Cr-Sect.00deg", skiprows=1 , unpack=True)

fig, ax = plt.subplots()

ax.loglog(nmols, crsec)

ax.set_xlim(1e26, 1e14)

ax.set_xlabel('number of molecules along solar ray [cm^-2]')
ax.set_ylabel('KD cross sec, cm^2')

fig.suptitle('K-term for CO2 FUV. Zenith angle is 0 deg')

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
