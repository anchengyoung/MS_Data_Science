#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  1 15:10:53 2022

@author: anchengyoung
"""
data = load_and_shuffle_data()

#%% Q6
from collections import Counter

def word_counter(bag_of_words):
    bag = bag_of_words[:-1]
    return Counter(bag)

#%% Q7
from sklearn.model_selection import train_test_split

X = []
y = []
for review in data:
    X.append(word_counter(review))
    y.append(review[-1])

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, shuffle=False)

#%% Q8

def pegasos_8(X, y, lmbda, num_epoch):
    
    w = {}
    t = 0
    epoch = 0
    
    while epoch <= num_epoch:
    
        for j in range(len(y)):
            t += 1
            eta = 1 / (t * lmbda)
            
            if y[j] * dotProduct(w, X[j]) < 1:
                increment(w, -eta * lmbda, w)
                increment(w, eta * y[j], X[j])
            else:
                increment(w, -eta * lmbda, w)        
        epoch += 1
    
    return w

#%% Q9

def pegasos_9(X, y, lmbda, num_epoch):
    
    s = 1
    t = 1
    W = {}
    epoch = 0
    
    while epoch <= num_epoch:
        
        for j in range(len(y)):
            t += 1
            eta = 1 / (t * lmbda)
            s = (1 - eta * lmbda) * s
            
            if s * y[j] * dotProduct(W, X[j]) < 1:
                increment(W, eta * y[j] / s, X[j])   
        
        epoch += 1
        
    w = {}
    increment(w, s, W)
    return w

#%% Q10

from datetime import datetime

start = datetime.now()
slow = pegasos_8(X_train, y_train, 0.1, 2)
end = datetime.now()
print(end-start)


start = datetime.now()
fast = pegasos_9(X_train, y_train, 0.1, 2)
end = datetime.now()
print(end-start)

#%% Q11

def classification_error(w, X, y):
    
    pred = []
    for j in range(len(y)):
        f = dotProduct(w, X[j])
        if np.sign(f) == np.sign(y[j]):
            pred.append(0)
        else:
            pred.append(1)
    
    return sum(pred)/len(pred)

#%% Q12
import matplotlib.pyplot as plt

lmbda_list = [0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1]
errors = []

for lmbda in lmbda_list:
    w = pegasos_9(X_train, y_train, lmbda, 100)
    error = classification_error(w, X_test, y_test)
    errors.append(error)
    
plt.plot(np.log10(lmbda_list), errors)
plt.title("Classification Errors on Test Set")
plt.xlabel(r"$\log(\lambda)$")
plt.ylabel("Classification Error (%)")

#%% Kernel
import sklearn
from scipy.spatial.distance import cdist
import functools

#%% Q21

### Kernel function generators
def linear_kernel(X1, X2):
    """
    Computes the linear kernel between two sets of vectors.
    Args:
        X1 - an n1xd matrix with vectors x1_1,...,x1_n1 in the rows
        X2 - an n2xd matrix with vectors x2_1,...,x2_n2 in the rows
    Returns:
        matrix of size n1xn2, with x1_i^T x2_j in position i,j
    """
    return np.dot(X1,np.transpose(X2))
 
def RBF_kernel(X1,X2,sigma):
    """
    Computes the RBF kernel between two sets of vectors   
    Args:
        X1 - an n1xd matrix with vectors x1_1,...,x1_n1 in the rows
        X2 - an n2xd matrix with vectors x2_1,...,x2_n2 in the rows
        sigma - the bandwidth (i.e. standard deviation) for the RBF/Gaussian kernel
    Returns:
        matrix of size n1xn2, with exp(-||x1_i-x2_j||^2/(2 sigma^2)) in position i,j
    """
    return np.exp(-cdist(X1,X2,'sqeuclidean') / (2 * (sigma**2)))

def polynomial_kernel(X1, X2, offset, degree):
    """
    Computes the inhomogeneous polynomial kernel between two sets of vectors
    Args:
        X1 - an n1xd matrix with vectors x1_1,...,x1_n1 in the rows
        X2 - an n2xd matrix with vectors x2_1,...,x2_n2 in the rows
        offset, degree - two parameters for the kernel
    Returns:
        matrix of size n1xn2, with (offset + <x1_i,x2_j>)^degree in position i,j
    """
    return (offset + linear_kernel(X1, X2))**degree

#%% Q22

x_0 = np.array((-4, -1, 0, 2)).reshape(4,1)

linear_kernel(x_0, x_0)

#%% Q23

# Plot kernel machine functions
plot_step = .01
xpts = np.arange(-6, 6, plot_step).reshape(-1,1)
prototypes = np.array([-4,-1,0,2]).reshape(-1,1)

# Linear kernel
y = linear_kernel(prototypes, xpts) 
for i in range(len(prototypes)):
    label = "Linear@"+str(prototypes[i,:])
    plt.plot(xpts, y[i,:], label=label)
plt.legend(loc = 'best')
plt.show()

# Poly kernel
y = polynomial_kernel(prototypes, xpts, 1, 3) 
for i in range(len(prototypes)):
    label = "Linear@"+str(prototypes[i,:])
    plt.plot(xpts, y[i,:], label=label)
plt.legend(loc = 'best')
plt.show()

# RBF kernel
y = RBF_kernel(prototypes, xpts, 1) 
for i in range(len(prototypes)):
    label = "Linear@"+str(prototypes[i,:])
    plt.plot(xpts, y[i,:], label=label)
plt.legend(loc = 'best')
plt.show()

#%% Q24

class Kernel_Machine(object):
    def __init__(self, kernel, training_points, weights):
        """
        Args:
            kernel(X1,X2) - a function return the cross-kernel matrix between rows of X1 and rows of X2 for kernel k
            training_points - an nxd matrix with rows x_1,..., x_n
            weights - a vector of length n with entries alpha_1,...,alpha_n
        """

        self.kernel = kernel
        self.training_points = training_points
        self.weights = weights
        
    def predict(self, X):
        """
        Evaluates the kernel machine on the points given by the rows of X
        Args:
            X - an nxd matrix with inputs x_1,...,x_n in the rows
        Returns:
            Vector of kernel machine evaluations on the n points in X.  Specifically, jth entry of return vector is
                Sum_{i=1}^R alpha_i k(x_j, mu_i)
        """
        return self.kernel(self.training_points, X).T @ self.weights
    

k = functools.partial(RBF_kernel, sigma = 1)
points = np.array((-1, 0, 1)).reshape(3, 1)
alphas = np.array((1, -1, 1)).reshape(3, 1)
f = Kernel_Machine(k, points, alphas)

plt.plot(points, f.predict(points))

#%% Q25

data_train,data_test = np.loadtxt("krr-train.txt"),np.loadtxt("krr-test.txt")
x_train, y_train = data_train[:,0].reshape(-1,1),data_train[:,1].reshape(-1,1)
x_test, y_test = data_test[:,0].reshape(-1,1),data_test[:,1].reshape(-1,1)

plt.scatter(x_train, y_train)
plt.title("Training Data")
plt.show()

#%% Q26

def train_kernel_ridge_regression(X, y, kernel, l2reg):
    
    K = kernel(X, X)
    I = np.identity(X.size)
    alpha = np.linalg.inv(l2reg * I + K) @ y
    
    return Kernel_Machine(kernel, X, alpha)

#%% Q27

plot_step = .001
xpts = np.arange(0 , 1, plot_step).reshape(-1,1)
plt.plot(x_train,y_train,'o')
l2reg = 0.0001
for sigma in [.01,.1,1]:
    k = functools.partial(RBF_kernel, sigma=sigma)
    f = train_kernel_ridge_regression(x_train, y_train, k, l2reg=l2reg)
    label = "Sigma="+str(sigma)+",L2Reg="+str(l2reg)
    plt.plot(xpts, f.predict(xpts), label=label)
plt.legend(loc = 'best')
plt.ylim(-1,1.5)
plt.title("RBF Kernel with Fixed Regularization Parameter")
plt.show()

#%% Q28

plot_step = .001
xpts = np.arange(0 , 1, plot_step).reshape(-1,1)
plt.plot(x_train,y_train,'o')
sigma= .02
for l2reg in [.0001,.01,.1,2]:
    k = functools.partial(RBF_kernel, sigma=sigma)
    f = train_kernel_ridge_regression(x_train, y_train, k, l2reg=l2reg)
    label = "Sigma="+str(sigma)+",L2Reg="+str(l2reg)
    plt.plot(xpts, f.predict(xpts), label=label)
plt.legend(loc = 'best')
plt.ylim(-1,1.5)
plt.title("RBF Kernel with Fixed Sigma")
plt.show()

