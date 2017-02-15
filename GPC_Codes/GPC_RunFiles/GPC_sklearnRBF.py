# -*- coding: utf-8 -*-
"""
Created on Tue Aug 11 09:47:14 2015

@author: cbe117
"""

import numpy as np
#from sklearn import gaussian_process # removed 1/10/17
from sklearn.gaussian_process import GaussianProcessRegressor # added1/10/17
from sklearn.gaussian_process.kernels import RBF  # added1/10/17
import csv
import timeit
import sys
#print 'run files location',sys.argv[1]

print "Starting sklearnRBF"


# Adding 2/15/17. sklearnRBF was really bad, didn't have same problem with Matern or other packages or previous version of sklearn
# Using normalize_y=True in GPRegressor doesn't work, have to do it by self, not sure why.
scale_y = True
use_seed = True

#filesToRun = np.loadtxt( "C://Users//cbe117//School//DOE//Comparison//comparison2//filesToRunPython.csv" ,dtype="string",delimiter=',')
#filesToRun = np.loadtxt( "C://Users//cbe117//School//DOE//Comparison//GPC//GPC_Codes//GPC_RunFiles//filesToRunPython.csv" ,dtype="string",delimiter=',')
#filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//filesToRunsklearnRBF.csv"
#filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//OTLCircuit1//OTLCircuit1_D6_SS100_PS500_R5//RunFiles//filesToRunPython.csv"
if len(sys.argv) > 1:
    filesToRunName = sys.argv[1] # Changing this to take input
else:
    filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//Borehole03//Borehole03_D8_SS200_PS2000_R5//RunFiles//filesToRunsklearnRBF.csv"
    filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//Morris1//Morris1_D20_SS200_PS2000_R5//RunFiles//filesToRunsklearnRBF.csv"
    #filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//RGPP2_D2_B.7//RGPP2_D2_B.7_D2_SS50_PS2000_R5//RunFiles//filesToRunsklearnRBF.csv"
    filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//Borehole1357_03//Borehole1357_03_D4_SS100_PS2000_R5//RunFiles//filesToRunsklearnRBF.csv"

filesToRun = np.loadtxt( filesToRunName ,dtype="string",delimiter=',')
    # column 1 is input data
    # column 2 is prediction data, switched from 3
    # column 3 is filename data, switched from 2

def get_params2(a_gp):
    """ This function improves on the existing gp.get_params by adding the
    values for sigma2 and theta_ to the dictionary and returning it.
    Previously it was a method when I added it to the module, but this
    way is a lot less intrusive.
    """
    temp = a_gp.get_params()
    temp['sigma2'] = a_gp.sigma2
    temp['theta'] = a_gp.theta_
    return temp

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
    y = y.reshape(-1,1) # Adding this line 1/19/2016 since I'm getting error
    
    # Adding 2/15/17
    if scale_y:
        miny = np.min(y)
        maxy = np.max(y)
        y = (y - miny) / (maxy - miny)
    
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
    ypa = ypa.reshape(-1,1) # Adding this line 1/19/2016 since I'm getting error
    
    # Adding 2/15/17
    #if scale_y:
    #    ypa = (ypa - miny) / (maxy - miny)
    
    
    print 'Shapes are: ',X.shape,y.shape,xp.shape,ypa.shape,X.ndim,y.ndim,xp.ndim,ypa.ndim
    
    
    
    if use_seed:
        np.random.seed(int(filesToRun[i][7]))
        
        
        
    #gp = gaussian_process.GaussianProcess(theta0=1e-2, thetaL=1e-4, thetaU=1e-1)
    #gp = gaussian_process.GaussianProcess(theta0=1e-1, thetaL=1e-4, thetaU=20)
    #gp = gaussian_process.GaussianProcess(theta0=np.asarray([1e-1 for ijk in range(inputdim)]), thetaL=np.asarray([1e-4 for ijk in range(inputdim)]), thetaU=np.asarray([20 for ijk in range(inputdim)]))
    #gp = gaussian_process.GaussianProcess(theta0=np.asarray([1e-1 for ijk in range(inputdim)]))
    ## removing 1/10/17
    ##gp = gaussian_process.GaussianProcess(                          \
    ##    theta0=np.asarray([1e-1 for ijk in range(inputdim)]),       \
    ##    thetaL=np.asarray([1e-4 for ijk in range(inputdim)]),       \
    ##    #thetaU=np.asarray([200 for ijk in range(inputdim)]),        \ # changed this 4/19/16
    ##    thetaU=np.asarray([1e3 for ijk in range(inputdim)]),        \
    ##    #random_state = int(filesToRun[i][7]),
    ##        # Adding reshape since I'm getting error message 1/19/2016, solved all the error messages
    ##    #theta0=np.asarray([1e-1 for ijk in range(inputdim)]).reshape(-1,1),       \
    ##    #thetaL=np.asarray([1e-4 for ijk in range(inputdim)]).reshape(-1,1),       \
    ##    #thetaU=np.asarray([200 for ijk in range(inputdim)]).reshape(-1,1),        \
    ##    random_state = int(filesToRun[i][7]),
    ##    optimizer='Welch')        
    kernel = RBF(length_scale=np.asarray([1. for ijk in range(inputdim)])) # This and line below added 1/10/17
    #kernel = RBF(length_scale=np.asarray([1. for ijk in range(inputdim)]),  length_scale_bounds=(1e-16, 100000.0)) # This and line below added 1/10/17
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=10)#, normalize_y=False) # Need to give it restarts, just predicted zero when this argument was left out
    gp.fit(X, y)
    #print gp.get_params()
    
    ## y_pred, sigma2_pred = gp.predict(xp, eval_MSE=True) # removed 1/10/17
    y_pred, std_pred = gp.predict(xp, return_std=True)
    
    # Adding 2/15/17
    if scale_y:
        y_pred = y_pred * (maxy - miny) + miny
        std_pred = std_pred * (maxy - miny)
    print gp.kernel_
    #outstacked =  np.column_stack([xp,ypa,y_pred,sigma2_pred,np.sqrt(sigma2_pred)]) # removed 1/10/17
    outstacked =  np.column_stack([xp,ypa,y_pred,std_pred ** 2,std_pred]) # added 1/10/17
    #'C://Users//cbe117//School//DOE//Comparison//comparison2//runif//runif_1Preds_Pythonsklearn.csv'
    if inputdim==1:
        headers = "x,y,yp,yv,ysd"
    else:
        xheaders = ",".join(["x."+str(ii) for ii in range(1,inputdim+1)])
        headers = xheaders+",y,yp,yv,ysd"
    np.savetxt(filesToRun[i][3][1:-1],outstacked,delimiter=',',header=headers,comments='')
    #print i,filesToRun[i][2][1:-1]
    
    # make predictions on original data, only useful if nugget used
    #y_pred0, sigma2_pred0 = gp.predict(X, eval_MSE=True) # removed 1/10/17
    y_pred0, std_pred0 = gp.predict(X, return_std=True) # add 1/10/17
   # outstacked0 =  np.column_stack([X,y,y_pred0,sigma2_pred0,np.sqrt(sigma2_pred0)]) # removed 1/10/17
    outstacked0 =  np.column_stack([X,y,y_pred0,std_pred0**2, std_pred0])  # add 1/10/17
    np.savetxt(filesToRun[i][4][1:-1],outstacked0,delimiter=',',header=headers,comments='')

    # get parameters using my function
    #print gaussian_process
    #pars = gp.get_params2() # Removed this, using new function since it's easier.
    
    # 1/10/17 removing next four lines with pars because it doesn't work anymore
    #pars = get_params2(gp)
    #sigma2 = pars['sigma2'][0]
    #theta = pars['theta'][0]
    #delta = pars['nugget']
    
    # 1/10/17 adding these params
    sigma2 = -1234 # I don't see the sigma2 estimate???
    theta = gp.kernel.length_scale # It doesn't look like these get updated???
    delta = gp.alpha
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
