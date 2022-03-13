#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 14 00:10:18 2022

@author: taskerbliss
"""

import numpy as np
from scipy.interpolate import interp1d

file1 = "./Atmosph_Models/VIRA/venus_seiff1983_mdlAtm_00N0000.dat"
file2 = "./Atmosph_Models/UUV/UV_absorber_low_altitude.dat"

fmt = '%1.1f', '%1.5e', '%1.1f', '%1.5e'

vira_height, vira_concentration, vira_temperature, vira_pressure = np.loadtxt (file1, skiprows=2, unpack=True)

conc, height = np.loadtxt(file2, unpack=True)

concrat = interp1d(height, conc)

gas_h = vira_height #height
gas_p = vira_pressure/1000/1.013
gas_t = vira_temperature #temperature
gas_c = concrat(gas_h)*1e5 #concentration

vira_file_source = " VIRA file: " + file1 + "\n"
    
save_file = "./Atmosph_Models/UUV/" + "UUV_low_altitude_profile.dat"
header =  vira_file_source + str(1) + "  "\
        + str(len(gas_h)) + "\n" + " #height(km), pressure(atm), temperature(K), concentration (molecules/cm^2*km)"
        
np.savetxt (save_file, np.transpose ((gas_h, gas_p, gas_t, gas_c)), header=header, delimiter='  ', comments='', fmt=fmt)