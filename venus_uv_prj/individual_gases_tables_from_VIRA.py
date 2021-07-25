#!/usr/bin/env python3

# -*- coding: utf-8 -*-
"""
Created on Thu Jul 22 15:04:33 2021

@author: taskerbliss

Building tables for individual gaseous profiles from VIRA-file
"""

from scipy.interpolate import interp1d
import numpy as np
#import matplotlib.pyplot as plt

fmt = '%1.1f', '%1.5e', '%1.1f', '%1.5e'


vira_file = "./Atmosph_Models/VIRA/" + "venus_seiff1983_mdlAtm_00N0000.dat"
vira_height, vira_concentration, vira_temperature, vira_pressure = np.loadtxt (vira_file, skiprows=2, unpack=True)

#Available gases in ./Atmosph_Models directory are: CO2, H2O, CO, SO2, OCS, HF, HCl, O2, H2S
gases_names = ["CO2/co2.hq", "H2O/h2o.hq", "CO/co.hq", "SO2/so2.hq", "OCS/ocs.hq", "HF/hf.hq", "HCl/hcl.hq"]
#i = 0 # 0-CO2, 1-H2O, 2-CO, 3-SO2, 4-OCS, 5-HF, 6-HCl

for p in gases_names:
        f = "./Atmosph_Models/" + p
        pp = p.split('/')[0]
        ppp = pp.split('.')[0]
        height, mixratio = np.loadtxt (f, skiprows=1, unpack=True)

        #print (height)
        #print (mixratio)

        mix_rat = interp1d(height, mixratio)
    
        gas_h = vira_height #height
        gas_p = mix_rat(gas_h)*1e-6*vira_pressure #pressure
        gas_t = vira_temperature #temperature
        gas_c = mix_rat(gas_h)*1e-6*vira_concentration*1e5 #concentration
    
        save_file = "./Atmosph_Models/"+ pp + "/" + ppp + "_gas_profile.dat"
        header =  ppp + "\n number of lines:\n" + str(len(gas_h)) + "\n height(km), pressure(mbar), temperature(K), concentration (molecules/cm^2*km)"
        
        np.savetxt (save_file, np.transpose ((gas_h, gas_p, gas_t, gas_c)), header=header, delimiter='  ', fmt=fmt)
        

#x = np.linspace(0, 100, num=85, endpoint=True) #vary right border to 80 or 100 km if error
#plt.plot(mixratio, height, 'o', mix_rat(x), x, '-')
#plt.xscale('log')
#plt.ylim([0,100])
#plt.xlim([0.001, 1000])
#plt.show()


