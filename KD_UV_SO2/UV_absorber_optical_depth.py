#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 15 20:44:02 2021

@author: taskerbliss
"""

import matplotlib.pyplot as plt
import numpy as np
from scipy.ndimage.filters import gaussian_filter1d

#x = 1e7/(np.array([280, 300, 320, 350, 400]))
x = np.array([280, 300, 320, 350, 400])
#y70 = np.log10(np.array([1e-5, 1e-4, 5e-3, 1.5e-2, 8e-3]))
y60 = (np.array([2e-5, 2e-4, 1e-2, 4e-2, 1.6e-2]))

#ys = gaussian_filter1d(y, sigma=1)

#plt.plot(x, y70, color='b', label='high altitiude mode by Haus2015')
plt.plot(1e7/x, y60, color='r', label='low altitude mode by Haus2015' )
#plt.yscale('log')
plt.xlabel('wavelength, nm')
#plt.xlim([200, 400])
plt.ylabel('optical depth')
plt.title("optical depth of 1 km layer, UV-absorber, height=60 km and 70 km")
plt.legend()
plt.grid(color='g')
plt.show()

