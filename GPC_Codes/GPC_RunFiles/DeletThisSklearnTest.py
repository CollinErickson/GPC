# -*- coding: utf-8 -*-
"""
Created on Mon Nov 16 15:16:55 2015

@author: cbe117
"""

import numpy as np
from sklearn import gaussian_process

#X = np.asmatrix(np.arange(0, 1, 0.05)).reshape(20,1)
#Y = np.asmatrix(np.sqrt(X)+ np.cos(13*X)).reshape(20,1)
X = np.asmatrix(np.random.uniform(0,1,60)).reshape(30,2)
Y = np.asmatrix(np.random.uniform(0,1,30)).reshape(30,1)
inputdim=1

gp = gaussian_process.GaussianProcess(                          \
        theta0=np.asarray([1e-1 for ijk in range(inputdim)]),       \
        thetaL=np.asarray([1e-4 for ijk in range(inputdim)]),       \
        thetaU=np.asarray([200 for ijk in range(inputdim)]),        \
        random_state = 0)        
gp.fit(X, Y)