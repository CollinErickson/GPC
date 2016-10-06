print 1+2

"""import GPy
import numpy as np
X = np.matrix([0, .3, .5, .7, 1])
X = np.matrix(np.arange(0.01,1,.05))
y = 1/np.matrix(np.sin(3.14*X)) 
#y = np.matrix([0, -3, .5, .22, 10])
y += np.random.normal(0,1,y.shape)

inputdim=X.shape[0]

kernel = GPy.kern.RBF(input_dim=inputdim, variance=1., lengthscale=[1. for iii in range(inputdim)],ARD=True)
kernel = GPy.kern.RBF(input_dim=inputdim, ARD=True)

gp = GPy.models.GPRegression(X,y,kernel,normalizer=True) # added normalizer to make better

#gp.likelihood.variance = 1#e-4 # added 1/12/16, Max Zweissele says it will help with GPy issues.

print gp.param_array
gp.optimize(messages=False)
#print gp.param_array
gp.optimize_restarts(num_restarts = 5,  verbose=False)
print gp.param_array
"""
