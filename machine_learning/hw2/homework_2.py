#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import SGDClassifier
from sklearn.datasets import fetch_openml
from sklearn.preprocessing import StandardScaler

#%%

def load_data():
    #Loading the dataset
    print('loading the dataset')

    df = pd.read_csv('ridge_regression_dataset.csv', delimiter=',')
    X = df.values[:,:-1]
    y = df.values[:,-1]

    print('Split into Train and Test')
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=100, random_state=10)

    print("Scaling all to [0, 1]")
    X_train, X_test = feature_normalization(X_train, X_test)
    X_train = np.hstack((X_train, np.ones((X_train.shape[0], 1))))  # Add bias term
    X_test = np.hstack((X_test, np.ones((X_test.shape[0], 1))))
    
    return X_train, y_train, X_test, y_test

#%% Q4

def feature_normalization(train, test):
    
    train_normalized = np.zeros(train.shape)
    test_normalized  = np.zeros(test.shape)
    
    for i in range(len(train[0])):
        col = train[:,i]
        tmax = np.max(col)
        tmin = np.min(col)
        if tmax == tmin:
            train_normalized[:,i] = 2
        else:
            train_normalized[:,i] = (col - tmin) / (tmax - tmin)
            test_normalized[:,i] = (test[:,i] - tmin) / (tmax - tmin)
    
    for i in range(len(train_normalized[0])):
        tmin = np.min(train_normalized[:,i])
        if tmin > 1:
            np.delete(train_normalized, i, 1)
            np.delete(test_normalized, i, 1)
    
    return train_normalized, test_normalized        

#%% Q8

def compute_square_loss(X, y, theta):
    
    m = len(X)
    f = (X @ theta) - y
    loss = np.dot(f,f) / m
    
    return loss

#%% Q9

def compute_square_loss_gradient(X, y, theta):
    
    m = len(X)
    f = X @ theta - y
    grad = (2 * np.transpose(X) @ f) / m
    
    return grad

#%% Q10

def grad_checker(X, y, theta, epsilon=0.01, tolerance=1e-4):

    true_gradient = compute_square_loss_gradient(X, y, theta) #The true gradient
    num_features = theta.shape[0]
    approx_grad = np.zeros(num_features) #Initialize the gradient we approximate
    
    h = np.identity(num_features)
    J_plus = compute_square_loss(X, y, (theta + epsilon @ h))
    J_minus = compute_square_loss(X, y, (theta - epsilon @ h))
    approx_grad = J_plus - J_minus / (2 * epsilon)
    
    diff = true_gradient - approx_grad
    dist = np.sqrt(np.dot(diff.T, diff))
    if dist > tolerance:
        answer = False
    else:
        answer = True
    
    return answer

#%% Q11

def batch_grad_descent(X, y, alpha=0.1, num_step=1000, grad_check=False):

    num_instances, num_features = X.shape[0], X.shape[1]
    theta_hist = np.zeros((num_step + 1, num_features))  #Initialize theta_hist
    loss_hist = np.zeros(num_step + 1)  #Initialize loss_hist
    theta = np.zeros(num_features)  #Initialize theta
    
    for n in range(num_step+1):
        theta_hist[n] = theta
        loss_hist[n] = compute_square_loss(X, y, theta)
        grad = compute_square_loss_gradient(X, y, theta)
        if grad_check == True:
            assert grad_checker
        theta = theta - alpha * grad
    
    return theta_hist, loss_hist

#%% Q12

X_train, y_train, X_test, y_test = load_data()

steps = np.arange(1001)
alphas = [0.1, 0.5, 0.01, 0.05]
thetas = []
avg_loss = []

for alpha in alphas:
    theta, loss = batch_grad_descent(X_train, y_train, alpha=alpha,
                                     num_step=1000, grad_check=False)
    thetas.append(theta)
    avg_loss.append(loss)

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2,  figsize=(12,8))
axes = [ax1, ax2, ax3, ax4]

for i in range(len(alphas)):
    ax = axes[i]
    ax.plot(steps, avg_loss[i], label = alphas[i])
    ax.set_title("alpha = "+str(alphas[i]))
fig.suptitle("Average Square Loss on Training Set",
             fontsize='xx-large', y = 0.95)
fig.supylabel("Average Square Loss", x = 0.07)
fig.supxlabel("Number of Steps", y = 0.05)
plt.show()

#%% Q13

theta, loss = batch_grad_descent(X_train, y_train, alpha=0.05,
                                 num_step=1000, grad_check=False)
losses = []

for i in range(1001):
    loss = compute_square_loss(X_test, y_test, theta[i])
    losses.append(loss)

plt.plot(steps, losses)
plt.title("Average Square Loss on Testing Set\nwith alpha = 0.05")
plt.ylabel("Average Square Loss")
plt.xlabel("Number of Steps")
plt.show()

#%% Q15

def compute_regularized_square_loss_gradient(X, y, theta, lambda_reg):
    
    m = len(X)
    f = X @ theta - y
    ridge = 2 * lambda_reg * theta
    grad = ((2 * np.transpose(X) @ f) / m) + ridge
    
    return grad

#%% Q16

def regularized_grad_descent(X, y, alpha=0.05, lambda_reg=10**-2, num_step=1000):

    num_instances, num_features = X.shape[0], X.shape[1]
    theta = np.zeros(num_features) #Initialize theta
    theta_hist = np.zeros((num_step+1, num_features)) #Initialize theta_hist
    loss_hist = np.zeros(num_step+1) #Initialize loss_hist
    
    for n in range(num_step+1):
        theta_hist[n] = theta
        loss_hist[n] = compute_square_loss(X, y, theta)
        grad = compute_regularized_square_loss_gradient(X, y, theta, lambda_reg)
        theta = theta - alpha * grad
    
    return theta_hist, loss_hist

#%% Q17

lambdas = [1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1]

thetas = []
train_losses = []

for lmbda in lambdas:
    theta, loss = regularized_grad_descent(X_train, y_train, alpha=0.05,
                                           lambda_reg=lmbda, num_step=1000)
    thetas.append(theta)
    train_losses.append(loss)

test_losses = []

for theta in thetas:
    test_loss = []
    for i in range(1001):
        loss = compute_square_loss(X_test, y_test, theta[i])
        test_loss.append(loss)
    test_losses.append(test_loss)

fig, ((ax1, ax2, ax3), (ax4, ax5, ax6), (ax7, ax8, ax9)) = plt.subplots(3, 3, figsize=(12,8))
axes = [ax1, ax2, ax3, ax4, ax5, ax6, ax7, ax8, ax9]

for n in range(len(lambdas)):
    ax = axes[n]
    ax.plot(steps, train_losses[n], label = 'Train Loss')
    ax.plot(steps, test_losses[n], label = 'Test Loss')
    ax.set_title("Lambda = "+str(lambdas[n]))
    ax.legend()
    ax.label_outer()
fig.suptitle("Average Square Loss on Training and Testing Set",
             fontsize='xx-large', y = 0.95)
fig.supylabel("Average Square Loss", x = 0.08)
fig.supxlabel("Number of Steps", y = 0.05)
plt.show()

#%% Q18

last_train = []
last_test = []

for n in range(len(lambdas)):
    last_train.append(train_losses[n][-1])
    last_test.append(test_losses[n][-1])

plt.plot(lambdas, last_train, label = "Train")
plt.plot(lambdas, last_test, label = "Test")
plt.title("Average Square Loss at the End of Training")
plt.ylabel("Average Square Loss")
plt.xlabel(r"$\log(\lambda)$")
plt.xscale("log")
plt.legend()
plt.show()

#%% Q19

min_test = []

for n in range(len(lambdas)):
    min_test.append(np.min(test_losses[n]))

plt.plot(lambdas, last_train, label = "Average Train")
plt.plot(lambdas, last_test, label = "Average Test")
plt.plot(lambdas, min_test, label = "Minimum Test")
plt.title("Average Square Loss at the End of Training\nand the Minimum Square Loss on Testing Set")
plt.ylabel("Square Loss")
plt.xlabel(r"$\log(\lambda)$")
plt.xscale("log")
plt.legend()
plt.show()

#%% Q28

X_train, X_test, y_train, y_test = pre_process_mnist_01()

clf = SGDClassifier(loss='log', max_iter=1000, 
                    tol=1e-3,
                    penalty='l1', alpha=0.01, 
                    learning_rate='invscaling', 
                    power_t=0.5,                
                    eta0=0.01,
                    verbose=1)
clf.fit(X_train, y_train)

def classification_error(clf, X, y):
    
    y_hat = clf.predict(X)
    
    count = 0
    for i in range(len(y)):
        if y[i] != y_hat[i]:
            count += 1

    return count / len(y)
    
error = classification_error(clf, X_train, y_train)
print(error)
print(1 - clf.score(X_train, y_train))

#%% Q29

N_train = 100

alphas = np.linspace(1e-4, 1e-1, 10)

error_list = []
stdev_list = []
for alpha in alphas:
    clf = SGDClassifier(loss='log', max_iter=1000, 
                    tol=1e-3,
                    penalty='l1', alpha=alpha, 
                    learning_rate='invscaling', 
                    power_t=0.5,                
                    eta0=0.01,
                    verbose=0)
    errors = np.zeros(10)
    for i in range(10):
        X_train, y_train = sub_sample(N_train, X_train, y_train)
        clf.fit(X_train, y_train)
        error = classification_error(clf, X_test, y_test)
        errors[i] = error
    
    error_list.append(np.mean(errors))
    stdev_list.append(np.std(errors))


plt.errorbar(alphas, error_list, yerr = stdev_list, capsize = 6)
plt.title("Average Classification Error on Testing Set\nwith the Standard Deviation as Error Bars")
plt.ylabel("Classification Error")
plt.xlabel("Regularization Parameter")
plt.show()

#%% Q32

for alpha in alphas:
    clf = SGDClassifier(loss='log', max_iter=1000, 
                    tol=1e-3,
                    penalty='l1', alpha=alpha, 
                    learning_rate='invscaling', 
                    power_t=0.5,                
                    eta0=0.01,
                    verbose=0)

    X_train, y_train = sub_sample(N_train, X_train, y_train)
    clf.fit(X_train, y_train)
    
    scales = np.abs(clf.coef_).max()
    plt.imshow(np.reshape(clf.coef_, (28,28)),
               cmap=plt.cm.RdBu, vmax=scale, vmin=-scale)
    plt.title("alpha = "+str(np.round(alpha, 4)))
    plt.colorbar()
    plt.show()

