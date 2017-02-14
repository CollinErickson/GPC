import numpy as np

X = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/X.csv', skiprows=1)
Z = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Z.csv', skiprows=1)
XP = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/XP.csv', skiprows=1)
print X
print type(X)
print X.shape
print Z.shape
print XP.shape


from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF
kernel = RBF(length_scale=np.asarray([1. for ijk in range(len(X))]))
gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=10)
gp.fit(X, Z)
y_pred, std_pred = gp.predict(XP, return_std=True)
