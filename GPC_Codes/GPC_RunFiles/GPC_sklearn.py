# -*- coding: utf-8 -*-
"""
Created on Tue Aug 11 09:47:14 2015

@author: cbe117
"""

import numpy as np
from sklearn import gaussian_process
import csv
import timeit
import sys
#print 'run files location',sys.argv[1]

print "Starting Python"
    
#filesToRun = np.loadtxt( "C://Users//cbe117//School//DOE//Comparison//comparison2//filesToRunPython.csv" ,dtype="string",delimiter=',')
#filesToRun = np.loadtxt( "C://Users//cbe117//School//DOE//Comparison//GPC//GPC_Codes//GPC_RunFiles//filesToRunPython.csv" ,dtype="string",delimiter=',')
#filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//filesToRunPython.csv"
#filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//OTLCircuit1//OTLCircuit1_D6_SS100_PS500_R5//RunFiles//filesToRunPython.csv"
#filesToRunName = sys.argv[1] # Changing this to take input

if len(sys.argv) > 1:
    filesToRunName = sys.argv[1] # Changing this to take input
else:
    filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//Borehole03//Borehole03_D8_SS200_PS2000_R5//RunFiles//filesToRunsklearn.csv"
    filesToRunName = "//sscc//home//c//cbe117//Research//GPC//GPC_Output//Morris1//Morris1_D20_SS200_PS2000_R5//RunFiles//filesToRunsklearn.csv"


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
    
    print 'Shapes are: ',X.shape,y.shape,xp.shape,ypa.shape,X.ndim,y.ndim,xp.ndim,ypa.ndim
    
    #gp = gaussian_process.GaussianProcess(theta0=1e-2, thetaL=1e-4, thetaU=1e-1)
    #gp = gaussian_process.GaussianProcess(theta0=1e-1, thetaL=1e-4, thetaU=20)
    #gp = gaussian_process.GaussianProcess(theta0=np.asarray([1e-1 for ijk in range(inputdim)]), thetaL=np.asarray([1e-4 for ijk in range(inputdim)]), thetaU=np.asarray([20 for ijk in range(inputdim)]))
    #gp = gaussian_process.GaussianProcess(theta0=np.asarray([1e-1 for ijk in range(inputdim)]))
    gp = gaussian_process.GaussianProcess(                          \
        theta0=np.asarray([1e-1 for ijk in range(inputdim)]),       \
        thetaL=np.asarray([1e-4 for ijk in range(inputdim)]),       \
        #thetaU=np.asarray([200 for ijk in range(inputdim)]),        \ # changed this 4/19/16
        thetaU=np.asarray([1e3 for ijk in range(inputdim)]),        \
        #random_state = int(filesToRun[i][7]),
            # Adding reshape since I'm getting error message 1/19/2016, solved all the error messages
        #theta0=np.asarray([1e-1 for ijk in range(inputdim)]).reshape(-1,1),       \
        #thetaL=np.asarray([1e-4 for ijk in range(inputdim)]).reshape(-1,1),       \
        #thetaU=np.asarray([200 for ijk in range(inputdim)]).reshape(-1,1),        \
        random_state = int(filesToRun[i][7]),
        optimizer='Welch')        
    gp.fit(X, y)
    #print gp.get_params()
    
    y_pred, sigma2_pred = gp.predict(xp, eval_MSE=True)
    
    outstacked =  np.column_stack([xp,ypa,y_pred,sigma2_pred,np.sqrt(sigma2_pred)]) 
    #'C://Users//cbe117//School//DOE//Comparison//comparison2//runif//runif_1Preds_Pythonsklearn.csv'
    if inputdim==1:
        headers = "x,y,yp,yv,ysd"
    else:
        xheaders = ",".join(["x."+str(ii) for ii in range(1,inputdim+1)])
        headers = xheaders+",y,yp,yv,ysd"
    np.savetxt(filesToRun[i][3][1:-1],outstacked,delimiter=',',header=headers,comments='')
    #print i,filesToRun[i][2][1:-1]
    
    # make predictions on original data, only useful if nugget used
    y_pred0, sigma2_pred0 = gp.predict(X, eval_MSE=True)
    outstacked0 =  np.column_stack([X,y,y_pred0,sigma2_pred0,np.sqrt(sigma2_pred0)]) 
    np.savetxt(filesToRun[i][4][1:-1],outstacked0,delimiter=',',header=headers,comments='')

    # get parameters using my function
    #print gaussian_process
    #pars = gp.get_params2() # Removed this, using new function since it's easier.
    pars = get_params2(gp)
    sigma2 = pars['sigma2'][0]
    theta = pars['theta'][0]
    delta = pars['nugget']
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
