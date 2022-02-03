#!/usr/bin/env python3
# -*- coding: utf-8 -*-

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

#%% Q11 part 1

d_list = [2, 5, 10]
N_list = [100, 500, 900]

fig, axs = plt.subplots(3, 3, figsize=(18,12))
for ax in axs.flat:
    ax.label_outer()

et = {}
eg = {}

for d in d_list:
    
    et[d] = []
    eg[d] = []
    
    for N in range(d+1, 1000):
    
        x_train, y_train = draw_sample_with_noise(deg_true, a, N)
        x_test, y_test = draw_sample_with_noise(deg_true, a, N)
    
        X = get_design_mat(x_test, deg_true)
        g_x = X @ a
        X_1 = get_design_mat(x_train, d)
        X_2 = get_design_mat(x_test, d)
        b_hat = least_square_estimator(X_1, y_train)
        
        e_t = empirical_risk(X_1, y_train, b_hat)
        e_g = empirical_risk(X_2, y_test, b_hat)
        
        et[d].append(e_t)
        eg[d].append(e_g)
        
        if N in N_list:
            
            row = d_list.index(d)
            col = N_list.index(N)
            
            f_b = X_2 @ b_hat
            
            axs[row, col].scatter(x_test, y_test, s = 1, label = "Data")
            axs[row, col].plot(x_test, g_x, label = "g(x)")
            axs[row, col].plot(x_test, f_b, label = "f(x)")
            axs[row, col].set_title("d = "+str(d)+", N = "+str(N))
            axs[row, col].legend()

#%% Q11 part 2

for d in d_list:
    plt.plot(et[d], label = "d = "+str(d))
    plt.yscale("log")
plt.title("Training Error")
plt.legend()

for d in d_list:
    plt.plot(eg[d],  label = "d = "+str(d))
    plt.yscale("log")
plt.title("Generalization Error")
plt.legend()

#%% Q12
    
x_test, y_test = draw_sample_with_noise(deg_true, a, 1000)

estimator = {}

for d in d_list:
    
    estimator[d] = []
    
    for N in range(d+1, 1000):    
        
        x_train, y_train = draw_sample_with_noise(deg_true, a, N)
        X_1 = get_design_mat(x_train, d)
        b_hat_1 = least_square_estimator(X_1, y_train)
        X_2 = get_design_mat(x_test, d)
        b_hat_2 = least_square_estimator(X_2, y_test)

        est_error = empirical_risk(X_2, y_test, b_hat_1) - empirical_risk(X_2, y_test, b_hat_2)
        estimator[d].append(est_error)
        
fig, axs = plt.subplots(3, figsize=(12,8))
for d in d_list:
    pos = d_list.index(d)
    axs[pos].plot(estimator[d], label = "d = "+str(d))
    axs[pos].legend(fontsize = 'large')
plt.suptitle("Estimation Error", y = 0.92, size = 'xx-large')
plt.xlabel("N")
