# -*- coding: utf-8 -*-
"""
Created on Fri Sep 11 08:43:52 2015

@author: cbe117
"""

# -*- coding: utf-8 -*-
"""
Created on Tue Aug 11 09:47:14 2015

@author: cbe117
"""
#import sys
#sys.path.append("/home/me/mypy")

#sys.path.append("~/Research/GPC/GPC_Codes/GPC_RunFiles/GPy-master")


import numpy as np
#from sklearn import gaussian_process
import csv
import timeit
#from matplotlib import pyplot as plt
import cython
import GPy
import sys

print "Starting GPy"

#filesToRunName = "/sscc/home/c/cbe117/Research/GPC/GPC_Codes/GPC_RunFiles/filesToRunGPy.csv"
#filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//OTLCircuit1//OTLCircuit1_D6_SS100_PS500_R5//RunFiles//filesToRunPython.csv"
filesToRunName = sys.argv[1] # Changing this to take input
filesToRun = np.loadtxt( filesToRunName ,dtype="string",delimiter=',')
    # column 1 is input data
    # column 2 is prediction data, switched from 3
    # column 3 is filename data, switched from 2

numberToRun = len(filesToRun)-1
for i in range(1,numberToRun+1):
    print '\t',i
    
    # start timer
    starttime = timeit.time.clock()
    
    # open data file
    # 'C://Users//cbe117//School//DOE//Comparison//comparison2//runif//runif_1.csv'
    with open(filesToRun[i][1][1:-1],'r') as dest_f:
        data_iter = csv.reader(dest_f, 
                               delimiter = ',', 
                               quotechar = '"')
        data = [data for data in data_iter]
    data_array = np.asmatrix(data[1:], dtype = 'float')  
    data_arr = data_array#np.asarray(data_array[1:,:],dtype='float')
    inputdim = data_arr.shape[1]-2
    X = data_arr[:,1:-1]
    y = data_arr[:,-1]
    
    # open prediction file
    #'C://Users//cbe117//School//DOE//Comparison//comparison2//runif//runifPredPts.csv'
    with open(filesToRun[i][2][1:-1],'r') as dest_f:
        data_iter = csv.reader(dest_f, 
                               delimiter = ',', 
                               quotechar = '"')
        datap = [datap for datap in data_iter]
    datap = np.asmatrix(datap[1:],dtype='float')
    xp = datap[:,1:-1]
    ypa = datap[:,-1]
    #gp = gaussian_process.GaussianProcess(theta0=1e-2, thetaL=1e-4, thetaU=1e-1)
    #gp = gaussian_process.GaussianProcess(theta0=1e-1, thetaL=1e-4, thetaU=20)
    #gp = gaussian_process.GaussianProcess(theta0=np.asarray([1e-1 for ijk in range(inputdim)]), thetaL=np.asarray([1e-4 for ijk in range(inputdim)]), thetaU=np.asarray([20 for ijk in range(inputdim)]))
    #gp.fit(X, y)
    #print gp.get_params()
    
    # Replacing with GPy
    #kernel = GPy.kern.RBF(input_dim=inputdim, variance=1., lengthscale=[1. for iii in range(inputdim)],ARD=True) 
    kernel = GPy.kern.RBF(input_dim=inputdim, ARD=True) # Cut out unnecessary parameters 4/20/16
    #print(filesToRun)
    #print int(filesToRun[i,7])
    np.random.seed(int(filesToRun[i,7]))
    #gp = GPy.models.GPRegression(X,y,kernel)
    gp = GPy.models.GPRegression(X,y,kernel,normalizer=True) # added normalizer to make better

    gp.likelihood.variance = 1e-8 # added 1/12/16, Max Zweissele says it will help with GPy issues.

    gp.optimize(messages=False)
    #print gp.param_array
    gp.optimize_restarts(num_restarts = 5,  verbose=False)
    #print gp.param_array
    #y_pred, sigma2_pred = gp.predict(xp, eval_MSE=True)
    y_pred, sigma2_pred = gp.predict(np.asarray(xp))
    
    if any([sigcheck <0 for sigcheck in sigma2_pred]):
      print "Error 259783 in GPy, sigma2_pred is negative, can't take sqrt, number neg is ", sum([sigcheck <0 for sigcheck in sigma2_pred]),' of ', len(sigma2_pred)
      print "\tChanging these to have smallest variance"
      poss2p = [ss for ss in sigma2_pred if ss>0] # positive s2preds
      minposs2p = min(poss2p if len(poss2p)>0 else min(abs(sigma2_pred))) # set all nonpositive to smallest positive or smallest absolute value if none are positive
      sigma2_pred = [ss if ss>0 else minposs2p for ss in sigma2_pred]
    outstacked =  np.column_stack([xp,ypa,y_pred,sigma2_pred,np.sqrt(sigma2_pred)]) 
    #'C://Users//cbe117//School//DOE//Comparison//comparison2//runif//runif_1Preds_Pythonsklearn.csv'
    if inputdim==1:
        headers = "x,y,yp,yv,ysd"
    else:
        xheaders = ",".join(["x."+str(ii) for ii in range(1,inputdim+1)])
        headers = xheaders+",y,yp,yv,ysd"
    np.savetxt(filesToRun[i][3][1:-1],outstacked,delimiter=',',header=headers,comments='')
    #print 'GPy is print preds to',filesToRun[i][3][1:-1],filesToRun[i][3]
    #print i,filesToRun[i][2][1:-1]
    
    
    # make predictions on original data, only useful if nugget used
    #y_pred0, sigma2_pred0 = gp.predict(X, eval_MSE=True)
    y_pred0, sigma2_pred0 = gp.predict(np.asarray(X))
    if any([sigcheck0 <0 for sigcheck0 in sigma2_pred0]):
      print "Error 25978300 in GPy, sigma2_pred is negative, can't take sqrt, number neg is ", sum([sigcheck0 <0 for sigcheck0 in sigma2_pred0]),' of ', len(sigma2_pred0)
      print "\tChanging these to have smallest variance"
      poss2p0 = [ss0 for ss0 in sigma2_pred0 if ss0>0] # positive s2preds
      minposs2p0 = min(poss2p0 if len(poss2p0)>0 else min(abs(sigma2_pred0))) # set all nonpositive to smallest positive or smallest absolute value if none are positive
      sigma2_pred0 = [ss0 if ss0>0 else minposs2p0 for ss0 in sigma2_pred0]
    outstacked0 =  np.column_stack([X,y,y_pred0,sigma2_pred0,np.sqrt(sigma2_pred0)]) 
    # sqrt of negative causes error changing to be
    outstacked0 =  np.column_stack([X,y,y_pred0,sigma2_pred0,np.sqrt(sigma2_pred0)]) 
    np.savetxt(filesToRun[i][4][1:-1],outstacked0,delimiter=',',header=headers,comments='')
    
    #print gp.param_array
    # get parameters using my function
    #print gaussian_process
    
    pars = gp.param_array
    sigma2 = pars[0]
    theta = pars[1:-1]
    delta = pars[-1]
    outstackedpar =  np.asmatrix(np.append(theta.T,(sigma2,delta))) #np.column_stack([theta,sigma2,delta]) 
    parheaders = ""
    for ind in range(1,inputdim+1):
        parheaders +=  "beta." + str(ind) + ","
    parheaders += "sigma2,delta"
    #print pars['theta']
    #print outstackedpar
    #print filesToRun[i][6][1:-1]
    np.savetxt(filesToRun[i][6][1:-1],outstackedpar,delimiter=',',header=parheaders,comments='')
    


    #write out run time
    rtheaders="elapsed"
    outstackedtime = np.column_stack([timeit.time.clock()-starttime])
    np.savetxt(filesToRun[i][5][1:-1],outstackedtime,delimiter=',',header=rtheaders,comments='')
