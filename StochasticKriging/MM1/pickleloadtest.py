import pickle


with open('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/X.pkl', 'rb') as input:
  X= pickle.load(input)
  print type(X)

with open('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Z.pkl', 'rb') as input:
  Z= pickle.load(input)
with open('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/XP.pkl', 'rb') as input:
  XP= pickle.load(input)
  
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF
kernel = RBF(length_scale=np.asarray([1. for ijk in range(len(X))]))
gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=10)
gp.fit(X, Z)
y_pred, std_pred = gp.predict(XP, return_std=True)
