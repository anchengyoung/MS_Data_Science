#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb  1 13:29:48 2022

@author: anchengyoung
"""

cd "Documents/atNYU/Grad/2022 Spring/Machine Learning/Homework/hw1"

import numpy as np
import matplotlib.pyplot as plt

#%% Hands On

deg_true = 2
a = get_a(deg_true)

x_train, y_train = draw_sample(deg_true, a, 10)
x_test, y_test = draw_sample(deg_true, a, 1000)


#%% Q7

def least_square_estimator(X, y):
    N = X.size
    d = X.ndim - 1
    if N <= d:
        print("Error!")
    else:
        b_hat = np.linalg.inv(X.T @ X) @ X.T @ y
    return b_hat

#%% Q8

def empirical_risk(X, y, b):
    N = X.size
    matrix = X @ b - y
    
    risk = matrix.T @ matrix / (2 * N)
    
    return risk

#%% Q9

d = 5
X = get_design_mat(x_train, d)
b_hat = least_square_estimator(X, y_train)

print(b_hat)
print(a)

f_b = X @ b_hat

plt.scatter(x_train, y_train, label = "Data")
plt.plot(x_train, y_train, label = "g(x)")
plt.plot(x_train, f_b, label = "f(x)")

plt.legend()
plt.show()

#%% Q10

for i in range(10):
    d = i
    X = get_design_mat(x_train, d)
    b_hat = least_square_estimator(X, y_train)

    risk = empirical_risk(X, y_train, b_hat)
    print(d)
    print(risk)

#%% Q11


