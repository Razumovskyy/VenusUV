#!/usr/bin/env python3

# -*- coding: utf-8 -*-
"""
Created on Fri Sep 17 09:30:00 2021

@author: taskerbliss

Building table for individual gaseous profile of unknown UV-absorber
(temperature and pressure are from the same VIRA file,
number density is for high altitude mode from Haus2015)
"""

#from scipy.interpolate import interp1d
import numpy as np
#import matplotlib.pyplot as plt

"""Parameters for high-latitude mode of unknown UV-absorber from Haus2015"""

zb = 58. # lower base of peak altitude, [km]
zc = 12. # layer thickness at constant peak particle number, [km]
Hup = 1. # upper scale height, [km]
Hlo = 1. # lower scale height, [km] 
D = 10. # peak density, [molecules/cm^3]
UV_parameters = np.array([zb, zc, Hup, Hlo]) # set of parameters as numpy array 
###############################################################################
def UV_number_density(z, parameters, peak_density): # function that gives number density from height
    
    if (z>85) or (z<45):
        return 0
    
    if z>(parameters[0]+parameters[1]):
        return peak_density*np.exp(-(z-(parameters[0]+parameters[1]))/parameters[2])
    elif ((z>=parameters[0]) and (z<=(parameters[0]+parameters[1]))):
        return peak_density
    else:
        return peak_density*np.exp(-(parameters[0]-z)/parameters[3])
###############################################################################    

fmt = '%1.1f', '%1.5e', '%1.1f', '%1.5e'

"""reading VIRA-file"""

vira_file = "./RUN_KD_UP/Atmosph_Models/VIRA/" + "venus_seiff1983_mdlAtm_00N0000.dat"
vira_height, vira_concentration, vira_temperature, vira_pressure = np.loadtxt (vira_file, skiprows=2, unpack=True)

cloud_height = 50.0

gas_h = vira_height #height
#gas_p = mix_rat(gas_h)*1e-6*vira_pressure ! if you want partial pressure of gas
gas_p = vira_pressure/1000/1.013 # total pressure is in the output table
gas_t = vira_temperature #temperature
gas_c = []

for z in vira_height:
    gas_c.append(UV_number_density(z, UV_parameters, D)*1e5) #number density, [molecules/cm^2*km]
gas_c = np.array(gas_c)
    
vira_file_source = " VIRA file: " + vira_file + "\n"
  
save_file = "./RUN_KD_UP/Atmosph_Models/" + "UUV_gas_profile.dat"
header = "Profile of unknown UV-absorber." + vira_file_source + str(1) + "  "\
        + str(len(gas_h)) + "\n" + "height(km), pressure(atm), temperature(K), concentration (particles/cm^2*km)"
        
np.savetxt (save_file, np.transpose ((gas_h, gas_p, gas_t, gas_c)), header=header, delimiter='  ', comments='', fmt=fmt)
        
index_cloud = np.where(gas_h==cloud_height)[0][0]
     
save_file_cloud = "./RUN_KD_UP/Atmosph_Models/" + "UUV_gas_profile.dat_"+ str(int(cloud_height)) + "km.dat"
header_cloud = "Profile of unknown UV-absorber" + vira_file_source + str(1) + "   " + str(len(gas_h[index_cloud:])) + "\n" + "height(km), pressure(atm), temperature(K), concentration (molecules/cm^2*km)"
    
np.savetxt (save_file_cloud, np.transpose((gas_h[index_cloud:], gas_p[index_cloud:], 
                                             gas_t[index_cloud:], gas_c[index_cloud:])), 
 header=header_cloud, delimiter='  ', comments='', fmt=fmt)

      