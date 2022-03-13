# -*- coding: utf-8 -*-
"""
Created on Sun Mar 13 12:40:10 2022

@author: Mikhail Razumovskiy
"""

import numpy as np

file1 = 'SO2_above_clouds_Zhang2012.dat'
file2 = 'O3_Krasnopolsky_2013.dat'

vmr, height = np.loadtxt(file1, unpack=True)
fmt1 = '%1.1f', '%1.5f'
fmt2 = '%1.1f', '%1.2e'

np.savetxt('so2_vmr_Zhang2012.dat', np.transpose((height, vmr*1e-3)), 
           fmt=fmt1, header='SO2 above clouds Zhang2012')

#np.savetxt('o3_vmr.dat', np.transpose((height, vmr*1e-3)), 
    #       fmt=fmt2, header='O3 Krasnopolsky 2013 night 1-D model')