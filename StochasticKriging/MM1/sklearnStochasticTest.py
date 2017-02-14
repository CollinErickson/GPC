import numpy as np

X = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/X.csv', skiprows=1)
Z = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Z.csv', skiprows=1)
XP = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/XP.csv', skiprows=1)
nugprop = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/nugprop.csv', skiprows=1) / 1
X = X.reshape(-1, 1)
Z = Z.reshape(-1, 1)
XP = XP.reshape(-1, 1)
#print X
#print type(X)
#print X.shape
#print Z.shape
#print XP.shape


from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF
kernel = RBF(length_scale=np.asarray([1. for ijk in range(X.shape[1])]))
gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=10, alpha=nugprop)
gp.fit(X, Z)
y_pred, std_pred = gp.predict(XP, return_std=True)

np.savetxt('sklearn_YP.csv', y_pred, delimiter=',')
np.savetxt('sklearn_stdP.csv', std_pred, delimiter=',')
