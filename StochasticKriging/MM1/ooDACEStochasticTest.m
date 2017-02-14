
addpath(genpath('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//DACE//ooDACE'))
addpath('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//DACE//sqplab-0.4.5-distrib')


samples = csvread('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/X.csv', 1, 0);
values = csvread('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Z.csv',1,0);
x = csvread('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/XP.csv',1,0);
vars = csvread('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/vars.csv',1,0) / 1e3;
samples = csvread('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Xmean.csv',1,0);
values = csvread('/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Zmean.csv',1,0);
%vars = [.1; .1; .1; .1; .1; .1; .1];

% Sigma is the intrinsic covariance matirx (=variance of output values)
%%opts.Sigma = var(values,2);

        optimopts.DerivativeCheck = 'off';
        optimopts.Diagnostics = 'off';
        optimopts.Algorithm = 'active-set';
        optimopts.MaxFunEvals = 1000000;
        optimopts.MaxIter = 500;
        optimopts.GradObj = 'off';
        inputdim=1;
        opts.hpOptimizer = MatlabOptimizer( inputdim, 1, optimopts);
opts.Sigma = vars;
%%values = mean(values,2);
% the process variance sigma2 needs to be included in the MLE
opts.Algorithm = 'interior-point-convex';
opts.Algorithm = 'interiorPointConvex';
opts.Algorithm = 'active-set';
lb = -10; ub = 10;
opts.hpBounds = [lb ; ub];
opts.sigma20 = Inf; % optional, guess the initial value
opts.sigma2Bounds = [-2 ; 4]; % log10 scale
opts.generateHyperparameters0 = true; % optional, guess the initial value
% explicitly ask for BasicGaussianProcess (=Kriging without scaling)
opts.type = 'BasicGaussianProcess';
k = oodacefit( samples, values, opts );
[y s2] = k.predict(x);

