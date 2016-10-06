# -*- coding: utf-8 -*-
"""
Created on Fri Sep 11 07:48:53 2015

@author: cbe117
"""

# GPy example

#configure plotting
#%matplotlib inline
#%config InlineBackend.figure_format = 'svg'
import matplotlib;matplotlib.rcParams['figure.figsize'] = (8,5)

import numpy as np
from matplotlib import pyplot as plt
import GPy

X = np.random.uniform(-3.,3.,(20,1))
Y = np.sin(X) + np.random.randn(20,1)*0.05

kernel = GPy.kern.RBF(input_dim=1, variance=1., lengthscale=1.)


#type GPy.kern.<tab> here:
#GPy.kern.BasisFuncKernel?
#GPy.kern.BasisFuncKernel()

m = GPy.models.GPRegression(X,Y,kernel)

#from IPython.display import display
print(m)

m.plot()

m.optimize(messages=True)

m.optimize_restarts(num_restarts = 10)

#display(m)
_ = m.plot()

xp = X/2
y_pred, sigma2_pred = m.predict(xp)