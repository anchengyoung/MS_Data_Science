#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep 26 13:15:32 2021

@author: anchengyoung
"""

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

theta_array = np.arange(0.0, 1.0, 0.1)
alpha_array = np.arange(0.0, 1.0, 0.1)

def Log_Likelihood(X, Y):
    Z = np.log10(3150) +4*np.log10(X) + 2*np.log10(Y) +4*np.log10(1 - X - Y)
    return Z

theta, alpha = np.meshgrid(theta_array, alpha_array)

Z_value = Log_Likelihood(theta, alpha)

fig = plt.figure()
ax = Axes3D(fig)
ax.plot_surface(theta, alpha, Z_value)
ax.set_xlabel('theta')
ax.set_ylabel('alpha')
ax.set_zlabel('Log Likelihood')
plt.show()