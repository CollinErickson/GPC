# First run
SKout1 <- SKrun(reps=20, n0reps=5, n2=500)
plot_rmseprmse(SKout1)


set.seed(0)
SKout2 <- SKrun(reps=5, n0reps=5, n2=200)
plot_rmseprmse_strip(SKout2)
plot_rmseprmse_strip(SKout2, saveplot = T, post='200_20180313')
save_supplementary_table(SKout100, SS="5_100_20180313")

set.seed(1)
SKout100 <- SKrun(reps=5, n0reps=5, n2=100)
plot_rmseprmse_strip(SKout100, saveplot = F, post='100__20180313')
save_supplementary_table(SKout100, SS="5_100_20180313")
