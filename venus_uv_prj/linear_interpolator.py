#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 22 15:04:33 2021

@author: taskerbliss

Linear interpolator for binary data files
"""

from scipy.interpolate import BSpline 
import numpy as np

f = "./Atmosph_Models" + "/H2O/h2o.hq" # input name of gas, e.g. /OCS/ocs.hq. Available gases are:
# CO2, H2O, CO, SO2, HF, HCl, OCS, O2, H2S

height, mixratio = np.loadtxt (f, skiprows=1, unpack=True)

print (height)
print (mixratio)
