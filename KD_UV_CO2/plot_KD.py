#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 17 13:35:06 2021

plot from binary file with many coloumns

@author: taskerbliss
"""

import matplotlib.pyplot as plt

with open ("/home/taskerbliss/fortran/venus_uv/venus_uv_prj/FDO_FUPex_FUPap_Qex_Qap.A&B") as f:
    lines = f.readlines()
    x = [line.split()[0] for line in lines]
    y1 = [line.split()[1] for line in lines]
    y2 = [line.split()[2] for line in lines]
    y3 = [line.split()[3] for line in lines]
    y4 = [line.split()[4] for line in lines]
    y5 = [line.split()[5] for line in lines]
    y6 = [line.split()[6] for line in lines]
    y7 = [line.split()[7] for line in lines]
    y8 = [line.split()[8] for line in lines]
    
plt.plot (y2, x, color='r')
plt.plot (y3, x, color='b')

plt.legend(["upward flux exact", "upward flux approximate"])