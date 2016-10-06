
# test functions

RGPTest1 <- function(...) {
  rgp1 <- RGP(d=1,npd=10,return.all=T,use.GPfit=T)
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
    batch.name = "RGPTest1",
    reps=8, input.dim=1, input.ss=c(10,20), pred.ss=200,
    seed.start=1,seed.preds=101,seed.fit=301,
    func = rgp1$get,func.string=rgp1$string,
    DACE.include=F,JMP.include=F,Python.include=F,GPy.include=F,...
  )
}
RGPTest2 <- function(...) {
  rgp1 <- RGP(d=2,npd=10,return.all=T,use.GPfit=T)
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
    batch.name = "RGPTest2",
    reps=8, input.dim=2, input.ss=c(25,50), pred.ss=200,
    seed.start=1,seed.preds=101,seed.fit=301,
    func = rgp1$get,func.string=rgp1$string,
    DACE.include=F,JMP.include=F,Python.include=F,GPy.include=F,...
  )
}
RGPTest3 <- function(...) {
  rgp1 <- RGP(d=3,npd=6,return.all=T,use.GPfit=T)
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
    batch.name = "RGPTest3",
    reps=4, input.dim=3, input.ss=c(50,100), pred.ss=200,
    seed.start=1,seed.preds=101,seed.fit=301,
    func = rgp1$get,func.string=rgp1$string,
    DACE.include=T,JMP.include=F,Python.include=T,GPy.include=F,...
  )
}
RGPTest4 <- function(...) {
  rgp1 <- RGP(d=2,npd=7,return.all=T,use.GPfit=T)
  comparison.all.batch (#path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
    batch.name = "RGPTest4_AllFits",
    reps=4, input.dim=2, input.ss=c(30,60), pred.ss=200,
    seed.start=1,seed.preds=101,seed.fit=301,
    func = rgp1$get,func.string=rgp1$string,
    DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,...
  )
}
RGPTest5 <- function(...) {
  rgp1 <- RGP(d=2,npd=6,return.all=T,use.GPfit=T)
  comparison.all.batch (batch.name = "RGPTest5_AllFits",
                        reps=4, input.dim=2, input.ss=c(25,50), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,...
  )
}
RGPTest6 <- function(...) {
  rgp1 <- RGP(d=1,npd=10,return.all=T,use.GPfit=T)
  comparison.all.batch (batch.name = "RGPTest6_AllFits",
                        reps=4, input.dim=1, input.ss=c(8,16), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,...
  )
}
SSCCTest1 <- function(...) {
  rgp1 <- RGP(d=1,npd=10,return.all=T,use.GPfit=T)
  comparison.all.batch (batch.name = "SSCCTest1",
                        reps=4, input.dim=1, input.ss=c(8,16), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,...
  )
}
SSCCTest2 <- function(...) {
  rgp1 <- RGP(d=3,npd=5,return.all=T,use.GPfit=T)
  comparison.all.batch (batch.name = "SSCCTest2_3D",
                        reps=3, input.dim=3, input.ss=c(60,120), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,...
  )
}
SSCCTest3 <- function(...) {
  rgp1 <- RGP(d=4,npd=4,return.all=T,use.GPfit=T)
  comparison.all.batch (batch.name = "SSCCTest3_4D",
                        reps=3, input.dim=4, input.ss=c(60,120), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,...
  )
}
SSCCRun001 <- function(...) {
  rgp1 <- RGP(d=1,npd=5,return.all=T,use.GPfit=T,seed=1001)
  comparison.all.batch (batch.name = "SSCCRun001_1D",#"SSCCRuns20151130//SSCCRun001_1D", # Didn't work to add subfolder, just remove for now
                        reps=8, input.dim=1, input.ss=c(6,9,12,15,18), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun002 <- function(...) {
  rgp1 <- RGP(d=2,npd=5,return.all=T,use.GPfit=T,seed=1002)
  comparison.all.batch (batch.name = "SSCCRun002_2D",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80), pred.ss=200,
                        seed.start=2,seed.preds=102,seed.fit=302,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun003 <- function(...) {
  rgp1 <- RGP(d=3,npd=4,return.all=T,use.GPfit=T,seed=1003)
  comparison.all.batch (batch.name = "SSCCRun003_3D",
                        reps=6, input.dim=3, input.ss=c(55,85,115,130), pred.ss=200, #40,70,100
                        seed.start=3,seed.preds=103,seed.fit=303,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun004 <- function(...) {
  rgp1 <- RGP(d=4,npd=4,return.all=T,use.GPfit=T,seed=1004)
  comparison.all.batch (batch.name = "SSCCRun004_4D",
                        reps=4, input.dim=4, input.ss=c(200), pred.ss=200,  # 60,80,100,120,140,160,180,200
                        seed.start=4,seed.preds=104,seed.fit=304,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun005 <- function(...) {
  rgp1 <- RGP(d=5,npd=3,return.all=T,use.GPfit=T,seed=1005)
  comparison.all.batch (batch.name = "SSCCRun005_5D",
                        reps=3, input.dim=5, input.ss=c(75,100,125,150,175,200), pred.ss=200, # 75,100,125,150,175,200
                        seed.start=5,seed.preds=105,seed.fit=305,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun006 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1006)
  comparison.all.batch (batch.name = "SSCCRun006_6D",
                        reps=3, input.dim=6, input.ss=c(80,105,130,155,180,205,230,255,280), pred.ss=200,  # 80,105,130,155,180,205,230,255,280
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun006.oc <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1006)
  comparison.all.batch (batch.name = "SSCCRun006_6D",
                        reps=3, input.dim=6, input.ss=c(80,105,130), pred.ss=200,
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        only.compare=T,...
  )
}
SSCCRun007 <- function(...) {
  rgp1 <- RGP(d=1,npd=5,return.all=T,use.GPfit=T,seed=1001,betas=rep(0,1))
  comparison.all.batch (batch.name = "SSCCRun007_1D_0B",#"SSCCRuns20151130//SSCCRun001_1D", # Didn't work to add subfolder, just remove for now
                        reps=8, input.dim=1, input.ss=c(6,12,18), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun008 <- function(...) {
  rgp1 <- RGP(d=2,npd=5,return.all=T,use.GPfit=T,seed=1002,betas=rep(0,2))
  comparison.all.batch (batch.name = "SSCCRun008_2D_0B",
                        reps=8, input.dim=2, input.ss=c(20,40,60), pred.ss=200,
                        seed.start=2,seed.preds=102,seed.fit=302,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun009 <- function(...) {
  rgp1 <- RGP(d=3,npd=4,return.all=T,use.GPfit=T,seed=1003,betas=rep(0,3))
  comparison.all.batch (batch.name = "SSCCRun009_3D_0B",
                        reps=6, input.dim=3, input.ss=c(40,70,100), pred.ss=200,
                        seed.start=3,seed.preds=103,seed.fit=303,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun010 <- function(...) {
  rgp1 <- RGP(d=4,npd=4,return.all=T,use.GPfit=T,seed=1004,betas=rep(0,4))
  comparison.all.batch (batch.name = "SSCCRun010_4D_0B",
                        reps=4, input.dim=4, input.ss=c(60,85,110), pred.ss=200,
                        seed.start=4,seed.preds=104,seed.fit=304,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun011 <- function(...) {
  rgp1 <- RGP(d=5,npd=3,return.all=T,use.GPfit=T,seed=1005,betas=rep(0,5))
  comparison.all.batch (batch.name = "SSCCRun011_5D_0B",
                        reps=3, input.dim=5, input.ss=c(75,100,125), pred.ss=200,
                        seed.start=5,seed.preds=105,seed.fit=305,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun012 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1006,betas=rep(0,6))
  comparison.all.batch (batch.name = "SSCCRun012_6D_0B",
                        reps=3, input.dim=6, input.ss=c(80,105,130), pred.ss=200,
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun013 <- function(...) {
  rgp1 <- RGP(d=1,npd=5,return.all=T,use.GPfit=T,seed=1001,betas=rep(1,1))
  comparison.all.batch (batch.name = "SSCCRun013_1D_1B",#"SSCCRuns20151130//SSCCRun001_1D", # Didn't work to add subfolder, just remove for now
                        reps=8, input.dim=1, input.ss=c(6,12,18), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun014 <- function(...) {
  rgp1 <- RGP(d=2,npd=5,return.all=T,use.GPfit=T,seed=1002,betas=rep(1,2))
  comparison.all.batch (batch.name = "SSCCRun014_2D_1B",
                        reps=8, input.dim=2, input.ss=c(20,40,60), pred.ss=200,
                        seed.start=2,seed.preds=102,seed.fit=302,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun015 <- function(...) {
  rgp1 <- RGP(d=3,npd=4,return.all=T,use.GPfit=T,seed=1003,betas=rep(1,3))
  comparison.all.batch (batch.name = "SSCCRun015_3D_1B",
                        reps=6, input.dim=3, input.ss=c(40,70,100), pred.ss=200,
                        seed.start=3,seed.preds=103,seed.fit=303,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun016 <- function(...) {
  rgp1 <- RGP(d=4,npd=4,return.all=T,use.GPfit=T,seed=1004,betas=rep(1,4))
  comparison.all.batch (batch.name = "SSCCRun016_4D_1B",
                        reps=4, input.dim=4, input.ss=c(60,85,110), pred.ss=200,
                        seed.start=4,seed.preds=104,seed.fit=304,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun017 <- function(...) {
  rgp1 <- RGP(d=5,npd=3,return.all=T,use.GPfit=T,seed=1005,betas=rep(1,5))
  comparison.all.batch (batch.name = "SSCCRun017_5D_1B",
                        reps=3, input.dim=5, input.ss=c(75,100,125), pred.ss=200,
                        seed.start=5,seed.preds=105,seed.fit=305,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun018 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1006,betas=rep(1,6))
  comparison.all.batch (batch.name = "SSCCRun018_6D_1B",
                        reps=3, input.dim=6, input.ss=c(80,105,130), pred.ss=200,
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun019 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1019,betas=c(0,0,0,1,1,1))
  comparison.all.batch (batch.name = "SSCCRun019_6D_000111B",
                        reps=3, input.dim=6, input.ss=c(80,105,130), pred.ss=200,
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun020 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1020,betas=c(0,0,0,2,2,2))
  comparison.all.batch (batch.name = "SSCCRun020_6D_000222B",
                        reps=3, input.dim=6, input.ss=c(80,105,130), pred.ss=200,
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun021 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1021,betas=c(1,1,1,2,2,2))
  comparison.all.batch (batch.name = "SSCCRun021_6D_111222B",
                        reps=3, input.dim=6, input.ss=c(80,105,130), pred.ss=200,
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun022 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1022,betas=c(0,0,1,1,2,2))
  comparison.all.batch (batch.name = "SSCCRun022_6D_001122B",
                        reps=3, input.dim=6, input.ss=c(80,105,130), pred.ss=200,
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = rgp1$get,func.string=rgp1$string,
                        #DACE.include=T,JMP.include=F,Python.include=T,GPy.include=T,
                        ...
  )
}
SSCCRun023 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1023,betas=c(0,0,0,1,1,1))
  comparison.all.batch (batch.name = "SSCCRun023_6D_000111B",
                        reps=3, input.dim=6, input.ss=c(50,100,150,200,250,300,350,400,450,500), pred.ss=200,
                        seed.start=23,seed.preds=123,seed.fit=323,
                        func = rgp1$get,func.string=rgp1$string,
                        GPfit.include=F,
                        ...
  )
}
SSCCRun024 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1024,betas=c(0,0,0,2,2,2))
  comparison.all.batch (batch.name = "SSCCRun024_6D_000222B",
                        reps=3, input.dim=6, input.ss=c(50,100,150,200,250,300,350,400,450,500,600,700), 
                        pred.ss=200,
                        seed.start=24,seed.preds=124,seed.fit=324,
                        func = rgp1$get,func.string=rgp1$string,
                        GPfit.include=F,
                        ...
  )
}
SSCCRun025 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1025,betas=c(1,1,1,2,2,2))
  comparison.all.batch (batch.name = "SSCCRun025_6D_111222B",
                        reps=3, input.dim=6, input.ss=c(50,100,150,200,250,300,350,400,450,500), pred.ss=200,
                        seed.start=25,seed.preds=125,seed.fit=325,
                        func = rgp1$get,func.string=rgp1$string,
                        GPfit.include=F,
                        ...
  )
}
SSCCRun026 <- function(...) {
  rgp1 <- RGP(d=6,npd=3,return.all=T,use.GPfit=T,seed=1026,betas=c(0,0,1,1,2,2))
  comparison.all.batch (batch.name = "SSCCRun026_6D_001122B",
                        reps=3, input.dim=6, input.ss=c(50,100,150,200,250,300,350,400,450,500), pred.ss=200,
                        seed.start=26,seed.preds=126,seed.fit=326,
                        func = rgp1$get,func.string=rgp1$string,
                        GPfit.include=F,
                        ...
  )
}
Borehole01 <- function(...) {
  comparison.all.batch (batch.name = "Borehole01",
                        reps=3, input.dim=8, input.ss=c(50,100,200,300,400,500,600), pred.ss=500,
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = borehole,func.string='borehole',
                        #GPfit.include=T,
                        ...
  )
}
Borehole01_1 <- function(...) {
  comparison.all.batch (batch.name = "Borehole01",
                        reps=3, input.dim=8, input.ss=c(50,100,200), pred.ss=500,
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = borehole,func.string='borehole',
                        GPfit.include=T,
                        ...
  )
}
Borehole01_2 <- function(...) {
  comparison.all.batch (batch.name = "Borehole01",
                        reps=3, input.dim=8, input.ss=c(600), pred.ss=500, # took off 300
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = borehole,func.string='borehole',
                        GPfit.include=T,laGP.include=F,mlegp.include=F,
                        DACE.include=F,GPy.include=F,Python.include=F, # for speed
                        ...
  )
}
Borehole01_3 <- function(...) {
  comparison.all.batch (batch.name = "Borehole01",
                        reps=3, input.dim=8, input.ss=c(500), pred.ss=500, # took off 400
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = borehole,func.string='borehole',
                        GPfit.include=T,laGP.include=F,mlegp.include=F,
                        DACE.include=F,GPy.include=F,Python.include=F, # for speed
                        ...
  )
}
Borehole02 <- function(...) {
  comparison.all.batch (batch.name = "Borehole02",
                        reps=5, input.dim=8, input.ss=c(200,500), #c(50,100,200,300,400,500,600)[1:7],  only ran 2 for JMP
                        pred.ss=500,
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = borehole,func.string='borehole',
                        #GPfit.include=T,
                        ...
  )
}
Borehole1357 <- function(...) {
  comparison.all.batch (batch.name = "Borehole1357",
                        reps=3, input.dim=4, input.ss=c(50,100,150,200,250,300), pred.ss=500,
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = function(xx){borehole(c(xx[1],.5,xx[2],.5,xx[3],.5,xx[4],.5))},
                        func.string='borehole1357',
                        #GPfit.include=T,
                        GPfit.controls=1,
                        ...
  )
}
Borehole1357_02 <- function(...) {
  comparison.all.batch (batch.name = "Borehole1357_02",
                        reps=5, input.dim=4, input.ss=c(50,100,150,200,250,300), pred.ss=500,
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = function(xx){borehole(c(xx[1],.5,xx[2],.5,xx[3],.5,xx[4],.5))},
                        func.string='borehole1357',
                        #GPfit.include=T,
                        ...
  )
}
SSCCRunRU01 <- function(...) {
  comparison.all.batch (batch.name = "SSCCRunRU01_1D_001122B",
                        reps=8, input.dim=1, input.ss=c(5,10,15,20), pred.ss=500,
                        seed.start=1,seed.preds=101,seed.fit=201,
                        func = 'r.u',func.string='RastriginUnit',
                        GPfit.include=T,
                        ...
  )
}
SSCCRunRU02 <- function(...) {
  comparison.all.batch (batch.name = "SSCCRunRU02_2D",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80,100), 
                        pred.ss=500,
                        seed.start=2,seed.preds=102,seed.fit=202,
                        func = 'r.u',func.string='RastriginUnit',
                        GPfit.include=T,
                        ...
  )
}
SSCCRunRU03 <- function(...) {
  comparison.all.batch (batch.name = "SSCCRunRU03_3D",
                        reps=3, input.dim=3, input.ss=c(30,45,60,75,90,105,120,135,150,165),#45,90,135),#30,75,120,165), 
                        pred.ss=500,
                        seed.start=3,seed.preds=103,seed.fit=203,
                        func = 'r.u',func.string='RastriginUnit',
                        GPfit.include=T,
                        ...
  )
}

# Starting new method using RGP.points function
RGPP_D1_B0 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D1_B0",
                        reps=8, input.dim=1, input.ss=c(6,9,12,15,18), pred.ss=200,
                        seed.start=1,seed.preds=101,seed.fit=301,
                        func = list('RGP.points',betas=0,corr.power=2),
                        ...
  )
}
RGPP_D2_B0 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D2_B0",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80), pred.ss=200,
                        seed.start=2,seed.preds=102,seed.fit=302,
                        func = list('RGP.points',betas=0,corr.power=2),
                        ...
  )
}
RGPP_D3_B0 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D3_B0",
                        reps=8, input.dim=3, input.ss=c(40,55,70,85,100,115,130), pred.ss=200,
                        seed.start=3,seed.preds=103,seed.fit=303,
                        func = list('RGP.points',betas=0,corr.power=2),
                        ...
  )
}
RGPP_D4_B0 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D4_B0",
                        reps=8, input.dim=4, input.ss=c(40,60,80,100,120,140,160,180,200), pred.ss=200, 
                        seed.start=4,seed.preds=104,seed.fit=304,
                        func = list('RGP.points',betas=0,corr.power=2),
                        ...
  )
}
RGPP_D5_B0 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D5_B0",
                        reps=4, input.dim=5, input.ss=c(75,100,125,150,175,200,225), pred.ss=200, 
                        seed.start=5,seed.preds=105,seed.fit=305,
                        func = list('RGP.points',betas=0,corr.power=2),
                        ...
  )
}
RGPP_D6_B0 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B0",
                        reps=4, input.dim=6, input.ss=c(105,130,155,180,205,230,255,280), pred.ss=200, 
                        seed.start=6,seed.preds=106,seed.fit=306,
                        func = list('RGP.points',betas=0,corr.power=2),
                        ...
  )
}
RGPP_D1_B1 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D1_B1",
                        reps=8, input.dim=1, input.ss=c(6,9,12,15,18), pred.ss=200,
                        seed.start=7,seed.preds=107,seed.fit=307,
                        func = list('RGP.points',betas=1,corr.power=2),
                        ...
  )
}
RGPP_D2_B1 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D2_B1",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80), pred.ss=200,
                        seed.start=8,seed.preds=108,seed.fit=308,
                        func = list('RGP.points',betas=1,corr.power=2),
                        ...
  )
}
RGPP_D3_B1 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D3_B1",
                        reps=8, input.dim=3, input.ss=c(30,40,55,70,85,100,115,130), pred.ss=200,
                        seed.start=9,seed.preds=109,seed.fit=309,
                        func = list('RGP.points',betas=1,corr.power=2),
                        ...
  )
}
RGPP_D4_B1 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D4_B1",
                        reps=8, input.dim=4, input.ss=c(40,60,80,100,120,140,160,180,200), pred.ss=200, 
                        seed.start=10,seed.preds=110,seed.fit=310,
                        func = list('RGP.points',betas=1,corr.power=2),
                        ...
  )
}
RGPP_D5_B1 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D5_B1",
                        reps=4, input.dim=5, input.ss=c(75,100,125,150,175,200,225), pred.ss=200, 
                        seed.start=11,seed.preds=111,seed.fit=311,
                        func = list('RGP.points',betas=1,corr.power=2),
                        ...
  )
}
RGPP_D6_B1 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B1",
                        reps=4, input.dim=6, input.ss=c(80,105,130,155,180,205,230,255,280), pred.ss=200, 
                        seed.start=12,seed.preds=112,seed.fit=312,
                        func = list('RGP.points',betas=1,corr.power=2),
                        ...
  )
}
RGPP_D1_B2 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D1_B2",
                        reps=8, input.dim=1, input.ss=c(6,9,12,15,18), pred.ss=200,
                        seed.start=13,seed.preds=113,seed.fit=313,
                        func = list('RGP.points',betas=2,corr.power=2),
                        ...
  )
}
RGPP_D2_B2 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D2_B2",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80), pred.ss=200,
                        seed.start=14,seed.preds=114,seed.fit=314,
                        func = list('RGP.points',betas=2,corr.power=2),
                        ...
  )
}
RGPP_D3_B2 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D3_B2",
                        reps=8, input.dim=3, input.ss=c(30,40,55,70,85,100,115,130), pred.ss=200,
                        seed.start=15,seed.preds=115,seed.fit=315,
                        func = list('RGP.points',betas=2,corr.power=2),
                        ...
  )
}
RGPP_D4_B2 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D4_B2",
                        reps=8, input.dim=4, input.ss=c(40,60,80,100,120,140,160,180,200), pred.ss=200, 
                        seed.start=16,seed.preds=116,seed.fit=316,
                        func = list('RGP.points',betas=2,corr.power=2),
                        ...
  )
}
RGPP_D5_B2 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D5_B2",
                        reps=4, input.dim=5, input.ss=c(75,100,125,150,175,200,225), pred.ss=200, 
                        seed.start=17,seed.preds=117,seed.fit=317,
                        func = list('RGP.points',betas=2,corr.power=2),
                        ...
  )
}
RGPP_D6_B2 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B2",
                        reps=4, input.dim=6, input.ss=c(80,105,130,155,180,205,230,255,280), pred.ss=200, 
                        seed.start=18,seed.preds=118,seed.fit=318,
                        func = list('RGP.points',betas=2,corr.power=2),
                        ...
  )
}
RGPP_D6_B000111 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B000111",
                        reps=4, input.dim=6, input.ss=c(100,200,300,400,500,600), pred.ss=500, 
                        seed.start=19,seed.preds=119,seed.fit=319,
                        func = list('RGP.points',betas=c(0,0,0,1,1,1),corr.power=2),
                        GPfit.include=F,
                        ...
  )
}
RGPP_D6_B000222 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B000222",
                        reps=4, input.dim=6, input.ss=c(100,200,300,400,500,600), pred.ss=500, 
                        seed.start=20,seed.preds=120,seed.fit=320,
                        func = list('RGP.points',betas=c(0,0,0,2,2,2),corr.power=2),
                        GPfit.include=F,
                        ...
  )
}
RGPP_D6_B111222 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B000222",
                        reps=4, input.dim=6, input.ss=c(100,200,300,400,500,600), pred.ss=500, 
                        seed.start=21,seed.preds=121,seed.fit=321,
                        func = list('RGP.points',betas=c(1,1,1,2,2,2),corr.power=2),
                        GPfit.include=F,
                        ...
  )
}
RGPP_D6_B001122 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B000222",
                        reps=4, input.dim=6, input.ss=c(100,200,300,400,500,600), pred.ss=500, 
                        seed.start=21,seed.preds=121,seed.fit=321,
                        func = list('RGP.points',betas=c(0,0,1,1,2,2),corr.power=2),
                        GPfit.include=F,
                        ...
  )
}
OTLCircuit1 <- function(...) {
  comparison.all.batch (batch.name = "OTLCircuit1",
                        reps=5, input.dim=6, input.ss= c(200,400), # c(100,200,300,400,500,600),  Only running 2 on JMP
                        pred.ss=500, 
                        seed.start=22,seed.preds=122,seed.fit=322,
                        func = otlcircuit,
                        #GPfit.include=F,
                        GPfit.controls=1,
                        ...
  )
}
Braninsc1 <- function(...) {
  comparison.all.batch (batch.name = "Braninsc1",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80,90,100), pred.ss=500, 
                        seed.start=23,seed.preds=123,seed.fit=323,
                        func = 'braninsc',
                        GPfit.include=T,
                        ...
  )
}
Detpep108d1 <- function(...) {
  comparison.all.batch (batch.name = "Detpep108d1",
                        reps=5, input.dim=8, input.ss=c(200,400), #c(200,300,400,500,600),  # only using these 2 on JMP
                        pred.ss=500, # mlegp crashed on 800
                        seed.start=24,seed.preds=124,seed.fit=324,
                        func = 'detpep108d',
                        #GPfit.include=F,laGP.include=T,
                        ...
  )
}
Franke2D1 <- function(...) {
  comparison.all.batch (batch.name = "Franke2D1",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80,90,100), pred.ss=500, 
                        seed.start=25,seed.preds=125,seed.fit=325,
                        func = 'franke2d',
                        GPfit.include=T,
                        ...
  )
}
Fried1 <- function(...) { # NO GOOD, MLEGP GIVES Error in solve.default(gp$invVarMatrix) :   system is computationally singular: reciprocal condition number = 2.17613e-16 
  comparison.all.batch (batch.name = "Fried1",
                        reps=4, input.dim=5, input.ss=c(25,50,75,100,125,150), pred.ss=200, 
                        seed.start=26,seed.preds=126,seed.fit=326,
                        func = 'fried',
                        #GPfit.include=F,
                        ...
  )
}


# Try Theta=5 which is Beta=.7
RGPP_D1_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D1_B.7",
                        reps=8, input.dim=1, input.ss=c(6,9,12,15,18), pred.ss=200,
                        seed.start=27,seed.preds=127,seed.fit=327,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}
RGPP_D2_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D2_B.7",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80)[4], pred.ss=200, # only do 50 for JMP
                        seed.start=28,seed.preds=128,seed.fit=328,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}
RGPP_D3_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D3_B.7",
                        reps=8, input.dim=3, input.ss=c(30,40,55,70,85,100,115,130), pred.ss=200,
                        seed.start=29,seed.preds=129,seed.fit=329,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}
RGPP_D4_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D4_B.7",
                        reps=8, input.dim=4, input.ss=c(40,60,80,100,120,140,160,180,200)[7], pred.ss=200,  # only 160 for JMP
                        seed.start=30,seed.preds=130,seed.fit=330,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}
RGPP_D5_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D5_B.7",
                        reps=4, input.dim=5, input.ss=c(75,100,125,150,175,200,225), pred.ss=200, 
                        seed.start=31,seed.preds=131,seed.fit=331,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}
RGPP_D6_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B.7",
                        reps=5,#4, # originally ran 4, trying to get it up to five now
                        input.dim=6, input.ss=c(200,300,400)[2], # only 300 for JMP #c(80,105,130,155,180,205,230,255,280), 
                        pred.ss=200, 
                        seed.start=32,seed.preds=132,seed.fit=332,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}


# Theta=20 is beta=1.3

RGPP_D1_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D1_B1.3",   # CHANGED BELOW TO [1] BC ONLY 6 WORKS FOR JMP
                        reps=8, input.dim=1, input.ss=c(6,9,12,15,18)[c(1,5)], pred.ss=200,
                        seed.start=13,seed.preds=113,seed.fit=313,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
RGPP_D2_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D2_B1.3",
                        reps=8, input.dim=2, input.ss=c(20,30,40,50,60,70,80)[4], pred.ss=200, # only 50 for JMP
                        seed.start=14,seed.preds=114,seed.fit=314,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
RGPP_D3_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D3_B1.3",
                        reps=8, input.dim=3, input.ss=c(30,40,55,70,85,100,115,130), pred.ss=200,
                        seed.start=15,seed.preds=115,seed.fit=315,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
RGPP_D4_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D4_B1.3",
                        reps=8, input.dim=4, input.ss=c(40,60,80,100,120,140,160,180,200)[7], pred.ss=200, # only 160 for JMP
                        seed.start=16,seed.preds=116,seed.fit=316,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
RGPP_D5_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D5_B1.3",
                        reps=4, input.dim=5, input.ss=c(75,100,125,150,175,200,225), pred.ss=200, 
                        seed.start=17,seed.preds=117,seed.fit=317,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
RGPP_D6_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D6_B1.3",
                        reps=5,#4, # originally ran 4, trying to get it up to five now 
                        input.dim=6, input.ss=c(200,300,400)[2], # only 300 for JMP #,c(80,105,130,155,180,205,230,255,280), 
                        pred.ss=200, 
                        seed.start=18,seed.preds=118,seed.fit=318,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
BraninscJMPExternal <- function(...) {
  comparison.all.batch (batch.name = "BraninscJMPExternal",
                        reps=3, input.dim=2, input.ss=c(20,40,60), pred.ss=500, 
                        seed.start=1000,seed.preds=1100,seed.fit=1200,
                        func = 'braninsc',
                        GPfit.include=F,laGP.include=F,DACE.include=F,
                        external.fits=c('JMP'),JMP.include=F,
                        stepstorun=1,
                        ...
  )
}

