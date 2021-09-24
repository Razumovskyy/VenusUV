#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 17 13:35:06 2021

plot from binary file with many columns

@author: taskerbliss
"""

import matplotlib.pyplot as plt
import numpy as np

path = "./Results/Calc-21Sept/"

FUV_names = ['CO2_30deg.50000-8E4', '5gas_30deg_400.50000-8E4']
MUV_names = ['CO2_SO2.33333-50', '6Gases.33333-50']
NUVa_names = ['SO2.31-33333', '6Gases.31-33333']
NUVb_names = ['UVX.25-31', '6Gases.25-31']

def cast (ar):
    for i in range(len(ar)):
        ar[i] = [float(item) for item in ar[i]]
    return np.array(ar)

def FQ_read (path_name):
    
    with open (path_name) as f:
        lines = f.readlines()
    
    data = [] #list of lists --- all data
    col = [] #list for one column
    
    for i in range (7): # z, and FDO, FUP, FUPap, Q, Qap -- for two angles
        for line in lines:
            col.append(line.split()[i])
        
        col1 = col.copy() # avoiding alias!! changing one list changes another!
        data.append(col1)
        col.clear()
    
    return data
    
z = []
FDO = []
FUP = []
Q = []

for n in MUV_names: #MUV_names, NUVa_names, NUVb_names
    
    z.append(FQ_read(path + n)[1])
    FDO.append(FQ_read(path + n)[4])
    FUP.append(FQ_read(path + n)[5])
    Q.append(FQ_read(path + n)[6])
    
z = cast(z)
Q = cast(Q)
FDO = cast(FDO)
FUP = cast(FUP)

def F_plot():
    
    fig, ax = plt.subplots()
    
    ax.plot(z[0], FDO[0], 'k--', label='downward flux CO2 & SO2 only')
    ax.plot(z[1], FDO[1], 'k', label='downward flux all gases')
    ax.plot(z[0], FUP[0], 'b--', label='upward flux CO2 & SO2 only')
    ax.plot(z[1], FUP[1], 'b', label='upward flux all gases')
    #ax.set_xlim(150,0)
    ax.set_xlabel("z, km")
    ax.set_ylabel("Flux, W/m^2")
    
    ax.grid(True)
    
    fig.suptitle("Fluxes from etalon simulations. Zenith angle 30 deg. 33333 - 50000 cm^-1")
    plt.legend()
    
    
def Q_plot():
    
    fig, ax = plt.subplots()
    
    ax.plot(z[0], Q[0], 'r--', label='heating rate CO2 & SO2 only')
    ax.plot(z[1], Q[1], 'r', label='heating rate all gases')
    ax.set_xlim(150, 0)
    ax.set_xlabel("z, km")
    ax.set_ylabel("Heating rate, K/day")
    
    ax.grid(True)
    
    fig.suptitle("Heating rates from etalon simulations. Zenith angle 30 deg. 33333 - 50000 cm^-1")
    plt.legend()

F_plot()    
#Q_plot()
