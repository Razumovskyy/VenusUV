#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 17 12:19:18 2021

template for plot from binary file

@author: taskerbliss
"""
import matplotlib.pyplot as plt

with open ("/home/taskerbliss/fortran/venus_uv/venus_uv_prj/Solar_Irradiance"+
           "/cosi_full_1nm_max.txt") as f:
               lines = f.readlines()
               x = [line.split()[0] for line in lines]
               y = [line.split()[1] for line in lines]

x_arr = []
y_arr = []

for item in x:
    x_arr.append(float(item))

for item in y:
    y_arr.append(float(item))
              
plt.plot(x_arr, y_arr, color='r', label='cosi_full_1nm_max.txt')
plt.xlabel ("wavelength")
plt.ylabel ("solar irradiance")
#plt.title ("")
plt.legend()
plt.show()        

