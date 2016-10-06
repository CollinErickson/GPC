import numpy as np
import GPy

def output(a):
    return np.sin(2*np.pi*a) #+ .1*np.sin(10*np.pi*a)

# See how seeds affect result when just large sin curve
np.random.seed(0) #  51 of 200 are negative variances
np.random.seed(1) # 137 of 200 
np.random.seed(2) # 0 of 200 
#np.random.seed(3) # 0 of 200 
#np.random.seed(4) # 55 of 200 
#np.random.seed(5) # 0 of 200 
#np.random.seed(6) # 1 of 200 



inputdim = 1
# Sample input is X
X = np.linspace(.05,.95,10)+np.random.uniform(-.05,.05,10)
X = np.asmatrix(X).reshape(10,1)
# output
y = output(X)
# Prediction points
xp = np.asmatrix(np.random.uniform(0,1,200)).reshape(200,1)
# These are the true values we are estimating
ypa = output(xp)


kernel = GPy.kern.RBF(input_dim=inputdim, variance=1., lengthscale=[1. for iii in range(inputdim)],ARD=True)


gp = GPy.models.GPRegression(X,y,kernel)


gp.optimize(messages=False)

gp.optimize_restarts(num_restarts = 5,  verbose=False)

y_pred, sigma2_pred = gp.predict(np.asarray(xp))

print 'Min sigma2_pred is',min(sigma2_pred)

if any([sigcheck <0 for sigcheck in sigma2_pred]):
  print "Error 259783 in GPy, sigma2_pred is negative, can't take sqrt, number neg is ", sum([sigcheck <0 for sigcheck in sigma2_pred]),' of ', len(sigma2_pred)
  print "\tChanging these to have zero variance"
  #sigma2_pred = [max(s2p,0) for s2p in sigma2_pred]
outstacked =  np.column_stack([xp,ypa,y_pred,sigma2_pred,np.sqrt(sigma2_pred)]) 
