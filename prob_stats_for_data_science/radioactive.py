#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 21 17:41:29 2021

@author: anchengyoung
"""

import numpy as np
import matplotlib.pyplot as plt

path = 'Documents/atNYU/Grad/Probability/Submission/hw8/radioactive_sample/'
sample_1 = np.genfromtxt(path+'radioactive_sample_1.txt')
sample_2 = np.genfromtxt(path+'radioactive_sample_2.txt')

#%% 4(a)

moving_avg_1 = []
for i in range(len(sample_1)):
    upto = sample_1[:i+1]
    avg = np.mean(upto)
    moving_avg_1.append(avg)

plt.plot(moving_avg_1)
print(moving_avg_1[-1])

#%% 4(b)

moving_avg_2 = []
for i in range(len(sample_2)):
    upto = sample_2[:i+1]
    avg = np.mean(upto)
    moving_avg_2.append(avg)

plt.plot(moving_avg_2)

#%% 4(d)

moving_median = []
for i in range(len(sample_2)):
    upto = sample_2[:i+1]
    median = np.median(upto)
    moving_median.append(median)

plt.plot(moving_median)
print(moving_median[-1])