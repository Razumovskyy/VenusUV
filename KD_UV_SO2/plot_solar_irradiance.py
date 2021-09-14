# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import matplotlib.pyplot as plt
import numpy as np
from scipy.ndimage.filters import gaussian_filter1d

solar_path = "./RUN_KD_UP/Solar_Irradiance/cosi_full_1nm_max.txt"
sec_path = "./RUN_KD_DO/UV_absorption/SO2_ManattLane1993.dat"

def acs_read(dat_path):
    
    with open (dat_path) as f:
               lines = f.readlines()
               lines.pop(0)
               lines.pop(0)
               x = [line.split()[0] for line in lines]
               y = [line.split()[1] for line in lines]

    x_arr = []
    y_arr = []

    for item in x:
        x_arr.append(float(item))

    for item in y:
        y_arr.append(float(item))
    
    xn = np.array(x_arr)
    yn = np.array(y_arr)
    
    return [xn, yn]
              
# =============================================================================
# def uv_acs_plot_nm():
#     
#     plt.plot(acs_read(path0)[0], acs_read(path0)[1], color='k', label='cosi')
# 
#     plt.xlim([200, 400])
#     plt.grid(color='g', linestyle='-', linewidth=0.5)
#     #plt.yscale('log')
#     plt.xlabel ("wavelength [nm]")
#     plt.ylabel ("Irradiance [W/m^2*um]")
#     plt.title ("Solar Irradiance")
#     plt.legend()
#     plt.show()    
# =============================================================================

x = 1e7/acs_read(solar_path)[0]
y = 1e3*1.93*((acs_read(solar_path)[0])**2)*(acs_read(solar_path)[1])/1e10

ysmoothed = gaussian_filter1d(y, sigma=5)

def uv_acs_plot_wavenum():
    
    fig, ax = plt.subplots()
    ax.semilogy (1e7/acs_read(sec_path)[0], acs_read(sec_path)[1], color='b', label='SO2 ACS from Manattlane1993')
    ax.set_ylabel("Absorption Cross Section [cm^2/molec]")
    ax.set_xlabel("Wavenumber [cm^-1]")
    ax.set_xlim([25000, 50000])
    
    ax2 = ax.twinx()
    ax2.plot(x, ysmoothed, 
            'r--', linewidth=1, label='Solar Irradiance by Cosi')
    ax2.set_xlabel("Wavenumber [cm^-1]")
    ax2.set_ylabel("Irradiance [mW/m^2*cm^-1]")
    ax2.set_ylim([0, 50])
    
    ax.grid(True, color='g')
    ax.legend(loc='upper right')
    ax2.legend(loc = 'upper left')
    fig.suptitle('SO2 M- and N-UV ACS with Solar Irradiance at Venus TOA')
    plt.show()
        
# =============================================================================
#     plt.plot(1e7/acs_read(path0)[0], 1e3*1.93*((acs_read(path0)[0])**2)*(acs_read(path0)[1])/1e10, color='k', label='cosi')
# 
#     plt.grid(color='g', linestyle='-', linewidth=0.5)
#     plt.xlim([25000, 52000])
#     #plt.ylim([0, 1e-17])
#     #plt.yscale('log')
#     plt.xlabel ("wavenumber [cm^-1]")
#     plt.ylabel ("Irradiance [mW/m^2*cm^-1]")
#     plt.title ("Solar Irradiance")
#     plt.legend()
#     plt.show()  
# =============================================================================

#uv_acs_plot_nm()
uv_acs_plot_wavenum()

