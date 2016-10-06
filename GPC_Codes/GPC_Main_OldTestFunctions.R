if (F) {
  test1 <- function() {
    comparison.create.data(#path.base,
      batch.name = "deletethis1",
      reps=2,input.dim=1,input.ss=11,pred.ss=100,
      seed.start=0,seed.preds=100,
      func = function(x){x*(1-x)}
    )
    comparison.run(#path.batch,
      batch.name = "deletethis1",
      reps=2,input.dim=1,
      GPfit.powers=c(1.95,2),mlegp=T)
    comparison.compare(#path.batch,
      batch.name = "deletethis1",
      reps=2,input.dim=1,
      GPfit.powers=c(1.95,2),mlegp=T)
  }
  test.py <- function() {
    system(paste0('python C://Users//cbe117//School//DOE//Comparison//comparison2//comparison2_python.py'))
  }
  test.jmp <- function() {
    system(paste0('"C://Program Files//SAS//JMPPRO//11//jmp.exe" C://Users//cbe117//School//DOE//Comparison//comparison2//loop_files6.jsl'))
    # C:\Program Files\SAS\JMPPRO\11\jmp.exe C:\Users\cbe117\School\DOE\Comparison\comparison2\loop_files6.jsl
  }
  test.DACE <- function() {
    #cmd.DACE <- "\"C:\Program Files\MATLAB\R2014a\bin\matlab.exe\" -nodisplay -nosplash -nodesktop -r \"run('C:\Users\cbe117\School\DOE\Comparison\comparison2\comparison2_DACE.m');exit;"
    #cmd.DACE <- "\"C://Program Files//MATLAB//R2014a//bin//matlab.exe\" -nodisplay -nosplash -nodesktop -r \"run('C://Users//cbe117//School//DOE//Comparison//comparison2//comparison2_DACE.m');exit;"
    #system(cmd.DACE)
    
  }
  
  test.all <- function() {
    
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethis3",
                    reps=8, input.dim=1, input.ss=11, pred.ss=100,
                    seed.start=0,seed.preds=100,
                    func = function(x){sin(x)^2})
    
  }
  
  
  test.all2 <- function() {
    
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisrastu",
                    reps=8, input.dim=1, input.ss=10, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'r.u')
    
  }
  test.all3 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisrastu2D",
                    reps=8, input.dim=2, input.ss=20, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'r.u')
  }
  test.all4 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisrastu2Daj",
                    reps=8, input.dim=2, input.ss=35, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'r.u',only.compare=T)
  }
  test.all5 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes1D",
                    reps=8, input.dim=1, input.ss=10, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'phughes',only.compare=F)
  }
  test.all6 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes1D",
                    reps=8, input.dim=1, input.ss=10, pred.ss=500,
                    seed.start=0,seed.preds=100,
                    func = 'phughes',only.compare=F)
  }
  test.all7 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes2D",
                    reps=8, input.dim=2, input.ss=25, pred.ss=500,
                    seed.start=0,seed.preds=100,
                    func = 'phughes',only.compare=F)
  }
  test.all8 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes2D_40",
                    reps=8, input.dim=2, input.ss=40, pred.ss=500,
                    seed.start=0,seed.preds=100,
                    func = 'phughes',only.compare=F)
  }
  test.all9 <- function() {
    
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethissin",
                    reps=8, input.dim=1, input.ss=11, pred.ss=100,
                    seed.start=0,seed.preds=100,
                    func = function(x){sin(2*pi*x)},
                    only.compare=T)
    
  }
  test.all10 <- function() {
    
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethis3",
                    reps=8, input.dim=1, input.ss=11, pred.ss=100,
                    seed.start=0,seed.preds=100,
                    func = function(x){sin(x)^2},
                    only.compare=T)
    
  }
  test.all11 <- function() {
    
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes3D_80",
                    reps=2, input.dim=3, input.ss=80, pred.ss=300,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    only.compare=T)
    
  }
  test.all11 <- function() {
    
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes4D_80",
                    reps=2, input.dim=4, input.ss=80, pred.ss=300,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    only.compare=F)
  }
  test.all12 <- function() {
    
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes4D_120",
                    reps=5, input.dim=4, input.ss=120, pred.ss=300,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    only.compare=T)
  }
  test.all13 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes2D_20",
                    reps=8, input.dim=2, input.ss=20, pred.ss=200,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    only.compare=F)
  }
  test.all14 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes1D_15_2",
                    reps=4, input.dim=1, input.ss=15, pred.ss=100,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    only.compare=F)
  }
  test.all15 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes4D_20_1",
                    reps=4, input.dim=4, input.ss=20, pred.ss=100,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    only.compare=F)
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes4D_20_2",
                    reps=4, input.dim=4, input.ss=20, pred.ss=100,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    only.compare=F)
  }
  test.all16 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes1D_15_paramsavetest",
                    reps=4, input.dim=1, input.ss=15, pred.ss=100,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    DACE.meanfuncs=c("0","1",'2'),DACE.corrfuncs=c("2","e",'sph'),
                    only.compare=F)
  }
  test.all17 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "deletethisphughes2D_25_paramsavetest",
                    reps=2, input.dim=2, input.ss=25, pred.ss=100,
                    seed.start=1000,seed.preds=1100,
                    func = 'phughes',
                    JMP.include=F,GPfit.include=F,
                    DACE.meanfuncs=c("0"),DACE.corrfuncs=c("2"),
                    only.compare=F)
  }
  pres.ex1 <- function(onco=F) {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "NPSpres_phughes1D_15",
                    reps=8, input.dim=1, input.ss=15, pred.ss=200,
                    seed.start=200,seed.preds=210,
                    func = 'phughes',
                    DACE.meanfuncs=c("0"),DACE.corrfuncs=c("2"),
                    only.compare=onco)
  }
  pres.ex2 <- function(onco=F,dace=F) {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "NPSpres_phughes5D_100",
                    reps=3, input.dim=5, input.ss=100, pred.ss=600,
                    seed.start=200,seed.preds=210,
                    func = 'phughes',
                    DACE.meanfuncs=c("0"),DACE.corrfuncs=c("2"),
                    only.compare=T)
  }
  NPStest1 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "NPStest_rastu_2D_25",
                    reps=8, input.dim=2, input.ss=25, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'r.u',only.compare=T,
                    DACE.include=F
                    )
  }
  NPStest2 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "NPStest_rastu_2D_35",
                    reps=8, input.dim=2, input.ss=35, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'r.u',only.compare=F,
                    DACE.include=F
    )
  }
  NPStest3 <- function() {
    comparison.all (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "NPStest_rastu_2D_20",
                    reps=3, input.dim=2, input.ss=20, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'r.u',only.compare=F,
                    DACE.include=F
    )
  }
}
if (F) {
  compallbatch.test1 <- function(oc=F,DACE=F,input.ss=c(15,30),...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                    batch.name = "CABT_rastu",
                    reps=3, input.dim=2, input.ss=input.ss, pred.ss=200,
                    seed.start=0,seed.preds=100,
                    func = 'r.u',only.compare=oc,
                    DACE.include=DACE,...
    )
  }
  compallbatch.test2 <- function(oc=F,DACE=F,...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "CABT_rastu1D",
                          reps=3, input.dim=1, input.ss=c(15,30), pred.ss=200,
                          seed.start=0,seed.preds=100,
                          func = 'r.u',only.compare=oc,
                          DACE.include=DACE,...
    )
  }
  compallbatch.test3 <- function(oc=F,DACE=F,...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "CABT_rastu3D",
                          reps=3, input.dim=3, input.ss=c(30,50), pred.ss=200,
                          seed.start=0,seed.preds=100,
                          func = 'r.u',only.compare=oc,
                          DACE.include=DACE,...
    )
  }
  compallbatch.test4 <- function(oc=F,DACE=F,...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "CABT_rastu5D",
                          reps=3, input.dim=5, input.ss=c(30,50), pred.ss=200,
                          seed.start=0,seed.preds=100,
                          func = 'r.u',only.compare=oc,
                          DACE.include=DACE,...
    )
  }
  compallbatch.test5 <- function(oc=F,DACE=F,...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "CABT_rastu5D",
                          reps=3, input.dim=5, input.ss=c(70,100), pred.ss=200,
                          seed.start=0,seed.preds=100,
                          func = 'r.u',only.compare=oc,
                          DACE.include=DACE,...
    )
  }
  compallbatch.test6 <- function(oc=F,DACE=T,...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "Borehole1",
                          reps=3, input.dim=8, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,
                          func = borehole,only.compare=oc,
                          DACE.include=DACE,...
    )
  }
  compallbatch.test7 <- function(oc=F,DACE=T,...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "Borehole1_seedsmatter",
                          reps=3, input.dim=8, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,
                          func = borehole,only.compare=oc,
                          DACE.include=DACE,...
    )
  }
  testfitseed1 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "FitSeedTest1",
                          reps=3, input.dim=4, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,
                          func = 'r.u',only.compare=F,
                          DACE.include=T,...
    )
  }
  testfitseed1_2 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "FitSeedTest1_2",
                          reps=3, input.dim=4, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,
                          func = 'r.u',only.compare=F,
                          DACE.include=T,...
    )
  }
  testfitseed1_3 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "FitSeedTest1_3_300",
                          reps=3, input.dim=4, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = 'r.u',only.compare=F,
                          DACE.include=T,...
    )
  }
  testfitseed2_1 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "FitSeedTest2_nofitseedgiven",
                          reps=3, input.dim=4, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,
                          func = 'r.u',only.compare=F,
                          DACE.include=T,...
    )
  }
  testfitseed2_2 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "FitSeedTest2_2_nofitseedgivenagain",
                          reps=3, input.dim=4, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,
                          func = 'r.u',only.compare=F,
                          DACE.include=T,...
    )
  }
  testfitseed2_3 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "FitSeedTest2_3_fitseed300",
                          reps=3, input.dim=4, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = 'r.u',only.compare=F,
                          DACE.include=T,...
    )
  }
  testfitseed2_4 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "FitSeedTest2_4_fitseed300again",
                          reps=3, input.dim=4, input.ss=c(30,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = 'r.u',
                          DACE.include=T,...
    )
  }
  testrandfunc_1 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFunc_1",
                          reps=3, input.dim=1, input.ss=c(10), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(10,1),
                          DACE.include=T,...
    )
  }
  testrandfunc_2 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFunc_2_2D_25_50",
                          reps=3, input.dim=2, input.ss=c(25,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(10,2),
                          DACE.include=T,...
    )
  }
  testrandfunc_3 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFunc_3_2D_25_50",
                          reps=3, input.dim=2, input.ss=c(25,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(10,2),
                          DACE.include=T,...
    )
  }
  testrandfunc_4 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFunc_4_5D",
                          reps=3, input.dim=5, input.ss=c(50,100), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(10,5),
                          DACE.include=T,...
    )
  }
  testrandfunc_5 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFunc_5_2D",
                          reps=3, input.dim=2, input.ss=c(25,40), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(10,2),
                          DACE.include=T,...
    )
  }
  testrandfuncFourier_1 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFuncFourier_1_2D",
                          reps=5, input.dim=2, input.ss=c(25,40), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(10,2),
                          DACE.include=T,...
    )
  }
  testrandfuncFourier_2 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFuncFourier_2_2D",
                          reps=5, input.dim=2, input.ss=c(25,40), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(5,2),
                          DACE.include=T,...
    )
  }
  testrandfuncFourier_3 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFuncFourier_3_1D",
                          reps=5, input.dim=1, input.ss=c(10,20), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(8,1),
                          DACE.include=T,...
    )
  }
  testrandfuncFourier_4 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFuncFourier_4_5D",
                          reps=3, input.dim=5, input.ss=c(50,100), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(8,5),
                          DACE.include=T,...
    )
  }
  testrandfuncFourier_5 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFuncFourier_5_2D",
                          reps=5, input.dim=2, input.ss=c(25,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(10,2),
                          DACE.include=T,...
    )
  }
  testrandfuncFourier_6 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "TestRandFuncFourier_6_2D",
                          reps=5, input.dim=2, input.ss=c(25,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(3,2),
                          DACE.include=T,...
    )
  }
  weirdhdtest_1 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdHDTest_1",
                          reps=5, input.dim=3, input.ss=c(30,60,100), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = 'r.u',
                          DACE.include=T,...
    )
  }
  weirdhdtest_2 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdHDTest_2_3D_2ru_1null",
                          reps=5, input.dim=3, input.ss=c(25,40,60,100), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = function(x){rastrigin.unit(x[1:2])},
                          DACE.include=T,...
    )
  }
  weirdhdtest_3 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdHDTest_3_2D_1ru_1phughes",
                          reps=5, input.dim=2, input.ss=c(25,40,60), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = function(x){rastrigin.unit(x[1])+phughes(x[2])},
                          DACE.include=T,...
    )
  }
  weirdhdtest_4 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdHDTest_4_3D_2ru_1phughes",
                          reps=5, input.dim=3, input.ss=c(35,65,100), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = function(x){rastrigin.unit(x[1:2])+phughes(x[3])},
                          DACE.include=T,...
    )
  }
  weirdhdtest_5 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdHDTest_5_4D_2ru_2phughes",
                          reps=5, input.dim=4, input.ss=c(50,80,120), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = function(x){rastrigin.unit(x[1:2])+phughes(x[3:4])},
                          DACE.include=T,...
    )
  }
  WeirdRFF_1 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdRFF_1_1D_1",
                          reps=5, input.dim=1, input.ss=c(5,10,15,20,25,30,40,50), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(hm=5,d=1),
                          DACE.include=T,...
    )
  }
  WeirdRFF_2 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdRFF_2_2D_2",
                          reps=5, input.dim=2, input.ss=c(25,40,60), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = getrandfuncnocoeffs(hm=5,d=2),
                          DACE.include=T,...
    )
  }
  WeirdRFF_3 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdRFF_3_2D_12",
                          reps=5, input.dim=2, input.ss=c(25,40,60), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = function(x){fn <- getrandfuncnocoeffs(hm=5,d=1);fn(x[1])+fn(x[2])},
                          DACE.include=T,...
    )
  }
  WeirdRPSF_1 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdRPSF_1_1D_1",
                          reps=5, input.dim=1, input.ss=c(5,10,20), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = function(x){fn <- RPSF();fn(x)},
                          DACE.include=T,...
    )
  }
  WeirdRPSF_2 <- function(...) {
    comparison.all.batch (path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                          batch.name = "WeirdRPSF_2_2D_12",
                          reps=5, input.dim=2, input.ss=c(20,35,50,65), pred.ss=200,
                          seed.start=1,seed.preds=101,seed.fit=300,
                          func = function(x){fn <- RPSF();fn(x[1])+fn(x[2])},
                          DACE.include=T,...
    )
  }
}
QuickTest <- function(...) {
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                        batch.name = "QuickTest",
                        reps=3, input.dim=1, input.ss=c(10,20), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = function(x){sin(2*pi*x)},
                        DACE.include=F,JMP.include=F,Python.include=F,GPy.include=F,...
  )
}
QuickTest2 <- function(...) {
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                        batch.name = "QuickTest_GPy",
                        reps=3, input.dim=1, input.ss=c(10,20), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = function(x){sin(2*pi*x)+x^2*cos(8*pi*x)},
                        DACE.include=F,JMP.include=F,Python.include=F,GPy.include=T,...
  )
}
QuickTest3 <- function(...) {
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                        batch.name = "QuickTest_sklearn",
                        reps=3, input.dim=1, input.ss=c(10,20), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = function(x){sin(2*pi*x)+x^2*cos(8*pi*x)},
                        DACE.include=F,JMP.include=F,Python.include=T,GPy.include=F,...
  )
}
QuickTest4 <- function(...) {
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
                        batch.name = "QuickTest_DACE",
                        reps=3, input.dim=1, input.ss=c(10,20), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = function(x){sin(2*pi*x)},
                        DACE.include=T,JMP.include=F,Python.include=F,GPy.include=F,...
  )
}
