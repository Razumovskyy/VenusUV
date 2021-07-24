#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 22 15:04:33 2021

@author: taskerbliss

Linear interpolator for mixing ratios of gases
"""

from scipy.interpolate import interp1d
import numpy as np
import matplotlib.pyplot as plt

f = "./Atmosph_Models" + "/H2S/h2s.hq" # input name of gas, e.g. /OCS/ocs.hq. Available gases are:
# CO2, H2O, CO, SO2, HF, HCl, OCS, O2, H2S

height, mixratio = np.loadtxt (f, skiprows=1, unpack=True)

print (height)
print (mixratio)

mix_rat = interp1d(height, mixratio)

x = np.linspace(0, 100, num=85, endpoint=True) #vary right border to 80 or 100 km if error
plt.plot(mixratio, height, 'o', mix_rat(x), x, '-')
plt.xscale('log')
plt.ylim([0,100])
plt.xlim([0.001, 1000])
plt.show()


