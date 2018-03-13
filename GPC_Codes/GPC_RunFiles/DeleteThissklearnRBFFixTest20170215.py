# -*- coding: utf-8 -*-
"""
Created on Fri Jan 29 11:06:59 2016

@author: cbe117
"""

import numpy as np

from sklearn.gaussian_process import GaussianProcessRegressor # added1/10/17
from sklearn.gaussian_process.kernels import RBF  # added1/10/17
from sklearn.gaussian_process.kernels import Matern  # added 1/11/17


import matplotlib.pyplot as plt

# See how seeds affect result 
#np.random.seed(0) # BAD
#np.random.seed(1) # BAD 
#np.random.seed(2) # BAD
#np.random.seed(3) # GOOD
#np.random.seed(4) # BAD 
#np.random.seed(5) # GOOD
#np.random.seed(6) # GOOD
#np.random.seed(7) # BAD
#np.random.seed(8) # BAD
#np.random.seed(9) # BAD



inputdim = 1
# Sample input is X, sample size is just 6
#X = [0.495175943,0.763997997,0.327034104,0.126950401,0.952418089,0.666102143]
#X = [0.735784054,0.820935337,0.222954747,0.548702121,0.026907899,0.39617717,0.908988789,0.211611032,0.564921256]
X = [0.932215585,0.071342164,0.658540043,0.509569026,0.782181343,0.682892499,
     0.157798399,0.274737116,0.391386956,0.319910162,0.849827111,0.480614135,
     0.004721655,0.776415615,0.334090274,0.585492747,0.998098989,0.172281858]
X = np.asmatrix(X).reshape(18,1)
# output
#y = [0.62140807,0.429765865,0.07240906,0.760016504,0.767846027,0.133323225]
#y = [0.346476318,0.525599683,0.203938028,0.555739745,0.968763325,0.370510977,0.685377653,0.266580679,0.487506551]
y = [0.046235553,0.955747647,0.332300276,0.518521485,0.188085536,0.302631291,0.892138916,0.786844609,0.661085122,0.740373902,0.11850481,0.554387305,0.997358907,0.194378279,0.725175706,0.423269807,0,0.880314483]
y = np.abs(np.sin(4*np.pi*X))
y = np.asmatrix(y).reshape(18,1)

#ymean = y.mean()
#y = y-ymean

# Prediction points, could be anything in zero to one
xp = np.random.uniform(0,1,200)
xp = np.asmatrix(xp).reshape(200,1)
ypa = np.abs(np.sin(4*np.pi*xp))
ypa = np.asarray(ypa).ravel()

if True:
    kernel = RBF(length_scale=np.asarray([1. for ijk in range(inputdim)])) # This and line below added 1/10/17
else:
    kernel = Matern(length_scale=np.asarray([1 for ijk in range(inputdim)]), nu=1.5) # Changed from RBF on 1/11/17
    
gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=10) # Need to give it restarts, just predicted zero when this argument was left out
gp.fit(X, y)
#print gp.get_params()

## y_pred, sigma2_pred = gp.predict(xp, eval_MSE=True) # removed 1/10/17
y_pred, std_pred = gp.predict(xp, return_std=True)
y_pred = (y_pred[:,0])


print(max(std_pred))
plt.scatter(xp,y_pred,c='blue')
plt.scatter(xp,y_pred+ std_pred, c='orange',s=12)
plt.scatter(X,y,c='yellow',s=52)
plt.show()
print np.mean(np.square(y_pred - ypa))