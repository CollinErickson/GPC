# First run
SKout1 <- SKrun(reps=20, n0reps=5, n2=500)
plot_rmseprmse(SKout1)



SKout2 <- SKrun(reps=5, n0reps=5, n2=200)
plot_rmseprmse_strip(SKout2)
plot_rmseprmse_strip(SKout2, saveplot = T, post='200')


SKout100 <- SKrun(reps=5, n0reps=5, n2=100)
plot_rmseprmse_strip(SKout100, saveplot = T, post='100')
