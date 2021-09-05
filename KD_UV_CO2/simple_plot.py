#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 17 12:19:18 2021

template for plot from binary file

@author: taskerbliss
"""
import matplotlib.pyplot as plt
import numpy as np

path0 = "./UV_absorption/CO2/"

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
    
    #plt.plot(acs_read(path0+"CO2_Bastien1985.dat")[0], acs_read(path0+"CO2_Bastien1985.dat")[1], color='r', label='Bastien1985')
    plt.plot(acs_read(path0+"CO2_Kuo2004.dat")[0], acs_read(path0+"CO2_Kuo2004.dat")[1], color='b', label='Kuo2004')
    #plt.plot(acs_read(path0+"CO2_LewisCarver1983.dat")[0], acs_read(path0+"CO2_LewisCarver1983.dat")[1], color='c', label='LewisCarver1983')
    #plt.plot(acs_read(path0+"CO2_Inn1953.dat")[0], acs_read(path0+"CO2_Inn1953.dat")[1], color='g', label='Inn1953')
    #plt.plot(acs_read(path0+"CO2_Rabalais1971.dat")[0], acs_read(path0+"CO2_Rabalais1971.dat")[1], color='m', label='Rabalais1971')
    #plt.plot(acs_read(path0+"CO2_Watanabe1953.dat")[0], acs_read(path0+"CO2_Watanabe1953.dat")[1], color='y', label='Watanabe1953')
    plt.plot(acs_read(path0+"CO2_Yoshino1996.dat")[0], acs_read(path0+"CO2_Yoshino1996.dat")[1], color='k', label='Yoshino1996')
    plt.plot(acs_read(path0+"CO2_Parkinson2003.dat")[0], acs_read(path0+"CO2_Parkinson2003.dat")[1], color='r', label='Parkinson2003')
    plt.plot(acs_read(path0+"CO2_Shemansky1972.dat")[0], acs_read(path0+"CO2_Shemansky1972.dat")[1], color='y', label='Shemansky1972')


    plt.xlim([120, 300])
    plt.yscale('log')
    plt.xlabel ("wavelength, nm")
    plt.ylabel ("ACS [cm^2/molec]")
    plt.title ("CO2 photoabsorption cross section")
    plt.legend()
    plt.show()    

def uv_acs_plot_wavenum():    
        
    #plt.plot(acs_read(path0+"CO2_Bastien1985.dat")[0], acs_read(path0+"CO2_Bastien1985.dat")[1], color='r', label='Bastien1985')
    plt.plot(1e7/acs_read(path0+"CO2_Kuo2004.dat")[0], acs_read(path0+"CO2_Kuo2004.dat")[1], color='b', label='Kuo2004')
    #plt.plot(acs_read(path0+"CO2_LewisCarver1983.dat")[0], acs_read(path0+"CO2_LewisCarver1983.dat")[1], color='c', label='LewisCarver1983')
    #plt.plot(acs_read(path0+"CO2_Inn1953.dat")[0], acs_read(path0+"CO2_Inn1953.dat")[1], color='g', label='Inn1953')
    #plt.plot(acs_read(path0+"CO2_Rabalais1971.dat")[0], acs_read(path0+"CO2_Rabalais1971.dat")[1], color='m', label='Rabalais1971')
    #plt.plot(acs_read(path0+"CO2_Watanabe1953.dat")[0], acs_read(path0+"CO2_Watanabe1953.dat")[1], color='y', label='Watanabe1953')
    plt.plot(1e7/acs_read(path0+"CO2_Yoshino1996.dat")[0], acs_read(path0+"CO2_Yoshino1996.dat")[1], color='k', label='Yoshino1996')
    plt.plot(1e7/acs_read(path0+"CO2_Parkinson2003.dat")[0], acs_read(path0+"CO2_Parkinson2003.dat")[1], color='r', label='Parkinson2003')
    plt.plot(1e7/acs_read(path0+"CO2_Shemansky1972.dat")[0], acs_read(path0+"CO2_Shemansky1972.dat")[1], color='y', label='Shemansky1972')


    plt.xlim([48000, 80000])
    plt.yscale('log')
    plt.xlabel ("wavenumber, cm^-1")
    plt.ylabel ("ACS [cm^2/molec]")
    plt.title ("CO2 photoabsorption cross section")
    plt.legend()
    plt.show()  

uv_acs_plot_nm()
#uv_acs_plot_wavenum()




