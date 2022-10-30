import numpy as np

cd 'Documents/atNYU/Grad/2022 Spring/Machine Learning/Homework/hw4'

X_train = np.genfromtxt('logistic-code/X_train.txt',delimiter=",")
X_test = np.genfromtxt('logistic-code/X_val.txt',delimiter=",")
y_train = np.genfromtxt('logistic-code/y_train.txt',delimiter=",")
y_test = np.genfromtxt('logistic-code/y_val.txt',delimiter=",")

y_train[4].reshape(1,-1)

num_instances, num_features = X_train.shape

theta = np.zeros(num_features)
theta[4] * X_train[4]

-y_train[42].reshape(1,-1) @ theta.reshape(1,-1) @ X_train[42]

len(X_train[42])

#%%



#%%

def f_objective(theta, X, y, l2_param=1):
    '''
    Args:
        theta: 1D numpy array of size num_features
        X: 2D numpy array of size (num_instances, num_features)
        y: 1D numpy array of size num_instances
        l2_param: regularization parameter

    Returns:
        objective: scalar value of objective function
    '''
    num_instances, num_features = X.shape
    summ = []
    
    for n in range(num_instances):
        summ.append(np.logaddexp(0, -y[n].reshape(1,-1) @ theta.reshape(1,-1) @ X[n]))
    
    objective = sum(summ) / num_instances
    objective += l2_param * np.inner(theta, theta)
    
    return objective
   
#%% 
from scipy.optimize import minimize

def fit_logistic_reg(X, y, objective_function, l2_param=1):
    '''
    Args:
        X: 2D numpy array of size (num_instances, num_features)
        y: 1D numpy array of size num_instances
        objective_function: function returning the value of the objective
        l2_param: regularization parameter
        
    Returns:
        optimal_theta: 1D numpy array of size num_features
    '''
    num_instances, num_features = X.shape
    
    for i in range(num_features):
        col = X[:,i]
        tmax = np.max(col)
        tmin = np.min(col)
        X[:,i] = (col - tmin) / (tmax - tmin)
    
    X = np.hstack((X, np.ones((num_instances, 1))))
    
    y = np.where(y == 0, -1, y)
    
    theta = np.zeros(num_features+1)
    optimal_theta = minimize(fun = objective_function, x0 = theta, args = (X, y, l2_param))
    
    return optimal_theta.x
    
#%%

optimal_theta = fit_logistic_reg(X_train, y_train, f_objective, l2_param=1)

#%%

def log_likelihood(theta, X, y):
    
    num_instances, num_features = X.shape
    
    for i in range(num_features):
        col = X[:,i]
        tmax = np.max(col)
        tmin = np.min(col)
        X[:,i] = (col - tmin) / (tmax - tmin)
    
    X = np.hstack((X, np.ones((num_instances, 1))))
    
    y = np.where(y == 0, -1, y)
    
    summ = []
    
    for n in range(num_instances):
        summ.append(np.logaddexp(0, -y[n].reshape(1,-1) @ theta.reshape(1,-1) @ X[n]))
    
    return sum(summ)

parameters = [1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10]
results = []

for param in parameters:
    theta = fit_logistic_reg(X_train, y_train, f_objective, l2_param = param)
    loglike = log_likelihood(theta, X_test, y_test)
    results.append(loglike)
    
#%%

import matplotlib.pyplot as plt

plt.plot(parameters, results)
plt.xscale('log')
plt.title("Log Likelihood on Validation Set")
plt.xlabel(r"$\lambda$")
plt.ylabel("Log Likelihood")

