import numpy as np

X = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/X.csv', skiprows=1)
Z = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Z.csv', skiprows=1)
XP = np.loadtxt('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/XP.csv', skiprows=1)
print X
print type(X)
print X.shape
print Z.shape
print XP.shape
