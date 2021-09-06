#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 17 13:35:06 2021

plot from binary file with many columns

@author: taskerbliss
"""

import matplotlib.pyplot as plt
import numpy as np

cloud_height = 70 # 50, 54, 60, 64, 70 km
path = "./Plot/"+ str(cloud_height) +"km_"+"FDO_FUP_FUPap_Q_Qap_ANGLES.dat"

def FQ_read (path_name):
    
    with open (path_name) as f:
        lines = f.readlines()
    
    data = [] #list of lists --- all data
    col = [] #list for one column
    
    for i in range (11): # z, and FDO, FUP, FUPap, Q, Qap -- for two angles
        for line in lines:
            col.append(line.split()[i])
        
        col1 = col.copy() # avoiding alias!! changing one list changes another!
        data.append(col1)
        col.clear()
    
    return data
    
z = FQ_read(path)[0]
z = np.array(z)

FDO = FQ_read(path)[6]
FDO = [float(item) for item in FDO]
FDO = np.array(FDO)
#FDO = np.log10(FDO)


FUP = FQ_read(path)[7]
FUP = [float(item) for item in FUP]
FUP = np.array(FUP)
#FUP = np.log10(FUP)


FUPap = FQ_read(path)[8]
FUPap = [float(item) for item in FUPap]
FUPap = np.array(FUPap)
#FUPap = np.log10(FUPap)

Q = FQ_read(path)[9]
Q = [float(item) for item in Q]
Q = np.array(Q)

Qap = FQ_read(path)[10]
Qap = [float(item) for item in Qap]
Qap = np.array(Qap)
    
def F_plot():
    
    plt.plot (FUP, z, color='b', label='upward flux', marker='|', linewidth = 3)
    plt.plot (FUPap, z, color='r', label='upward KD flux')
    plt.plot (FDO, z, color='k', label='downward flux')    
    #plt.plot (FUPap, z, color='r', label='upward KD flux')
    
    #plt.xlim([1e-10, 1e-5])
    #plt.xscale('log')
    plt.xlabel ("Flux, W/m^2")
    plt.ylabel ("z, km")
    plt.title ("Fluxes. Cloud deck is "+ str(cloud_height) + "km. Zenith angle 75")
    plt.legend()
    plt.show() 

def Q_plot():
    
    plt.plot (Q, z, color='b', label='heating rate', marker='|', linewidth = 3)
    plt.plot (Qap, z, color='r', label='KD heating rate')
      
    #plt.plot (FUPap, z, color='r', label='upward KD flux')
    
    #plt.xlim([1e-10, 1e-5])
    #plt.xscale('log')
    plt.xlabel ("Heating rate, K/day")
    plt.ylabel ("z, km")
    plt.title ("Heating rates. Cloud deck is "+ str(cloud_height) + "km. Zenith angle 75")
    plt.legend()
    plt.show() 
    
#F_plot()    
Q_plot()
