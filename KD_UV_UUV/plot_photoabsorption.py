#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 17 12:19:18 2021

template for plot from binary file

@author: taskerbliss
"""
import matplotlib.pyplot as plt
import numpy as np

path0 = "./RUN_KD_DO/UV_absorption/"

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
              
def uv_acs_plot_nm():
    
    plt.plot(acs_read(path0+"SO2_ManattLane1993.dat")[0], acs_read(path0+"SO2_ManattLane1993.dat")[1], color='b', label='ManattLane1993')

    plt.xlim([200, 400])
    plt.grid(color='g', linestyle='-', linewidth=0.5)
    plt.yscale('log')
    plt.xlabel ("wavelength [nm]")
    plt.ylabel ("ACS [cm^2/molec]")
    plt.title ("SO2 photoabsorption cross section")
    plt.legend()
    plt.show()    

def uv_acs_plot_wavenum():    
        
    plt.plot(1e7/acs_read(path0+"SO2_ManattLane1993.dat")[0], acs_read(path0+"SO2_ManattLane1993.dat")[1], color='b', label='ManattLane1993')

    plt.grid(color='g', linestyle='-', linewidth=0.5)
    plt.xlim([25000, 52000])
    #plt.ylim([0, 1e-17])
    plt.yscale('log')
    plt.xlabel ("wavenumber [cm^-1]")
    plt.ylabel ("ACS [cm^2/molec]")
    plt.title ("SO2 photoabsorption cross section")
    plt.legend()
    plt.show()  

#uv_acs_plot_nm()
uv_acs_plot_wavenum()




