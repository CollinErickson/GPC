%disp('Testing mpt');
%disp(filesToRunName);
%disp('End test');
%addpath('C:\Users\cbe117\School\DOE\GP_codes\DACE\dace');
%filesToRun = csvread( 'C://Users//cbe117//School//DOE//Comparison//comparison2//filesToRunDACE.csv',2,2 )
addpath('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//DACE//dace')
addpath('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//DACE//csvwrite_with_headers')
%filesToRunName = '//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//filesToRunDACE.csv';
% filesToRunName IS NOW PASSED IN FROM R!  
fid = fopen(filesToRunName);
filesToRun = textscan(fid,'%s%s%s%s%s%s%s%s%s%s','delimiter',',');
disp('Common problem: If you added a column to filesToRunDACE you have to add a scan to textscan line 5');
fclose(fid);

numberToRun = length(filesToRun{1})-1;
meancol = filesToRun{7};
corrcol = filesToRun{8};

for ii = 1:numberToRun
    % start timer
    tic
    
    fprintf('\t%d\n',ii)
    %# open data file
    %# 'C://Users//cbe117//School//DOE//Comparison//comparison2//runif//runif_1.csv'
    %  data_array = csvread('C://Users//cbe117//School//DOE//Comparison//comparison2//runif//runif_1.csv',1,1);
    temp = filesToRun{2}(ii+1);
    temp = temp{1};
    disp(temp(2:(length(temp)-1)))
    fprintf('%s\n',temp(2:(length(temp)-1)))
    %disp(filesToRun{2})
    %disp(temp)
    %disp('crashing here')
    data_array = csvread(temp(2:(length(temp)-1)),1,1);
    dims = size(data_array);
    inputdim = dims(2)-1;
    data_arr = data_array;%#np.asarray(data_array[1:,:],dtype='float')
    %inputdim = data_arr.shape[1]-2 % changed this 4/19/16
    S = data_arr(:,1:inputdim);
    Y = data_arr(:,inputdim+1);
    
    %theta = [10]; lob = [1e-1]; upb = [20];
    %theta = [1]; lob = [1e-4]; upb = [20];
    %theta = [1, 1]; lob = [1e-4, 1e-4]; upb = [20, 20];
    
    % Set starting theta, and lower and upper bounds
    theta = ones(1,inputdim)*1; 
    lob = ones(1,inputdim)*1e-4; 
    %upb = ones(1,inputdim)*20;
    upb = ones(1,inputdim)*1e3;

    % Determine mean function to use
    meanfunctemp0 = meancol(ii+1);
    meanfunctemp = meanfunctemp0{1};
    meanfuncstring = meanfunctemp(2:(length(meanfunctemp)-1));
    if any(strcmpi( meanfuncstring , {'regpoly0','0', 'constant','const','c'} ) )
        meanfunc = @regpoly0;
    elseif any(strcmpi( meanfuncstring , {'regpoly1','1','linear','lin','l' } ) )
        meanfunc = @regpoly1;
    elseif any(strcmpi( meanfuncstring , {'regpoly2','2','quadratic','quad','q'  } ) )
        meanfunc = @regpoly2;
    else
        meanfunc = @regpoly0;
        fprintf('Error: Mean function given is %s, setting meanfunc to @regpoly0',meanfuncstring);
    end
    
    % Determine correlation function to use
    corrfunctemp0 = corrcol(ii+1);
    corrfunctemp = corrfunctemp0{1};
    corrfuncstring = corrfunctemp(2:(length(corrfunctemp)-1));
    if any(strcmpi( corrfuncstring , {'correxp', 'exp','e' } ) )
        corrfunc = @correxp;
    elseif any(strcmpi( corrfuncstring , {'correxpg', 'expg','eg' } ) )
        corrfunc = @correxpg;
    elseif any(strcmpi( corrfuncstring , {'corrgauss', 'gauss', 'g', '2' } ) )
        corrfunc = @corrgauss;
    elseif any(strcmpi( corrfuncstring , {'corrlin','lin','l','1' } ) )
        corrfunc = @corrlin;
    elseif any(strcmpi( corrfuncstring , {'corrspherical', 'spherical','sph' } ) )
        corrfunc = @corrspherical;
    elseif any(strcmpi( corrfuncstring , {'corrspline', 'spline','spl','3' } ) )
        corrfunc = @corrspline;
    else
        corrfunc = @corrgauss;
        fprintf('Error: Correlation function given is %s, setting meanfunc to @corrgauss',corrfuncstring);
    end
    
    fprintf('mean func is: %s\n',meanfuncstring);
    fprintf('corr func is: %s\n',corrfuncstring);
    
    % set seed for the fit
    seedfit = filesToRun{10}(ii+1);
    seedfit = seedfit{1};
    rng(str2num(seedfit));
    
    %[dmodel, perf] = dacefit(S, Y, @regpoly0, @corrgauss, theta, lob, upb);
    [dmodel, perf] = dacefit(S, Y, meanfunc, corrfunc, theta, lob, upb);
    %horzcat(dmodel.theta,dmodel.sigma2,0,dmodel.beta)
    %disp(dmodel['theta'])
    
    
    temp2 = filesToRun{3}(ii+1);
    temp2 = temp2{1};
    datP = csvread(temp2(2:(length(temp2)-1)),1,1);
    %datP = csvread('C:\Users\cbe117\School\DOE\Comparison\comparison2\deletethisrastu2Daj\deletethisrastu2Daj_PredPts.csv',1,1);
    XP = datP(:,1:inputdim);%reshape(linspace(0,1,100),100,1);
    YPA = datP(:,inputdim+1);
    %[y, mse] = predictor([.5;], dmodel);
    [YP, MSEP] = predictor(XP, dmodel);
    %plot(XP,YP); hold on;
    %plot(S,Y,'+')
    %plot(XP,YP+2*sqrt(MSEP),'g')
    %plot(XP,YP-2*sqrt(MSEP),'g')

    datout = horzcat(XP,YPA,YP,MSEP,sqrt(MSEP));
    temp3 = filesToRun{4}(ii+1);
    temp3 = temp3{1};
    
    %disp('right here?')
    %disp(dmodel.beta)
    %disp('y/n?')
    %if inputdim == 1
    %    headers = {'x','y','yp','yv','ysd'};
    %    % switch theta to beta to agree with others
    %    %paramheaders = {'theta','sigma2','delta','mu'};
    %    paramheaders = {'beta','sigma2','delta','mu'};
    %else
        %headers = {'x','y','yp','yv','ysd'};
        headers = {'x.1'};
        %paramheaders = {'theta.1'};
        paramheaders = {'beta.1'};
        for jj = 2:inputdim
            headers = [headers,strcat('x.',num2str(jj))];
            %paramheaders = [paramheaders,strcat('theta.',num2str(jj))];
            paramheaders = [paramheaders,strcat('beta.',num2str(jj))];
        end
        headers = [headers,'y','yp','yv','ysd'];
        paramheaders = [paramheaders,'sigma2','delta','mu'];
        %paramheaders = [paramheaders,'sigma2','delta'];
            
    %end
    
    if func2str(meanfunc) == 'regpoly1'
        %paramheaders = [paramheaders,'mu1'];
        for ddd = 1:inputdim
            % Can't use beta since that is what theta is
            %paramheaders = [paramheaders,strcat('beta.',num2str(ddd))];
            paramheaders = [paramheaders,strcat('coeff.',num2str(ddd))];
        end
    elseif func2str(meanfunc) == 'regpoly2'
        disp('Problem with getting paramheaders for regpoly2 probably?')
        %paramheaders = [paramheaders,'mu1','mu2'];
        for ddd = 1:inputdim
            %paramheaders = [paramheaders,strcat('beta.',num2str(ddd))];
            paramheaders = [paramheaders,strcat('coeff.',num2str(ddd))];
        end
        for ddd = 1:inputdim
            for eee = ddd:inputdim
                %paramheaders = [paramheaders,strcat('beta.',num2str(ddd),num2str(eee))];
                paramheaders = [paramheaders,strcat('coeff.',num2str(ddd),num2str(eee))];
            end
        end
    end
    
    %for ddd = 1:inputdim
    %    paramheaders = [paramheaders,strcat('beta.',num2str(ddd))];
    %end
    
    %disp(headers)
    csvwrite_with_headers(temp3(2:(length(temp3)-1)),datout,headers)
    
    % write out params
    tempparam = filesToRun{6}(ii+1);
    tempparam = tempparam{1};
    paramsout = horzcat(dmodel.theta,dmodel.sigma2,0,dmodel.beta');
    %disp(filesToRun{6})
    %disp(paramheaders)
    %disp(paramsout)
    %disp(dmodel.beta)
    csvwrite_with_headers(tempparam(2:(length(tempparam)-1)),paramsout,paramheaders);
    
    
    
    
    % Predict on OPPs
    [YP0, MSEP0] = predictor(S, dmodel);

    datout0 = horzcat(S,Y,YP0,MSEP0,sqrt(MSEP0));
    temp4 = filesToRun{5}(ii+1);
    temp4 = temp4{1};
    csvwrite_with_headers(temp4(2:(length(temp4)-1)),datout0,headers)
    %csvwrite_with_headers( ??? ,paramsout,paramheaders)
    
    % end timer
    temprt = filesToRun{9}(ii+1);
    temprt = temprt{1};
    rtheaders = {'elapsed'};
    rtout = horzcat(toc);
    %disp(rtheaders)
    %disp(rtout)
    csvwrite_with_headers(temprt(2:(length(temprt)-1)),rtout,rtheaders)

end
