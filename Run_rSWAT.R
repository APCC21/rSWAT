rm(list=ls()) # Be careful! This removes all object in your working environment.

##########################################
#### Step 1. Working Environments Setting.
##########################################
EnvList <- Set.Working.Environment(envfile = "D:/Dropbox/R-packages/rswat/rSWAT_Training.yaml")

################# SWAT Run Using Observed data ################################
Swat.Observation.Run (EnvList, SimType="Calibration", Syear=2005, Eyear=2008)
Swat.Observation.Rch.Analysis (EnvList, SimType="Calibration", Syear=2005)
Swat.Observation.Run (EnvList, SimType="Calibration", Syear=1983, Eyear=2012)
Swat.Observation.Rch.Analysis (EnvList, SimType="Calibration", Syear=1983)

################# SWAT Run Using  Climate Change ##############################
Swat.CChange.Run (EnvList)
Swat.CChange.Rch.Analysis (EnvList)


################# SWAT Run Using  Seasonal forecast ###########################
#fiyearmon = "1983-12"
for(i in 1983:2012){
  for(j in 1:12){
    fiyearmon = sprintf("%4d-%02d", i, j)
    Swat.SForecast.Run (EnvList, fiyearmon)
    Swat.SForecast.Rch.Analysis (EnvList, fiyearmon)
  }
}


############### Calculating Forecasting Skill Score : Separated Obs Data #####
SwatFcstDir = EnvList$SwatFcstDir
esms = c("MME", "MIN", "MAX")
indir = "E:/SWAT-Climate/CliWARA/SWAT/SForecast/Analysis"
outdir = indir
SetWorkingDir(outdir)

options(stringsAsFactors = FALSE)

for(k in 1:3){
  esm = esms[k]
  for(i in 1:12){
    for(j in 1:6){

      dirlist = list.dirs(indir, full.names = F)
      dirlist = dirlist[which(nchar(dirlist) > 0)]

      # Read Observed EDI
      obsfile = file.path("H:/SWAT-Climate/CliWARA/SWAT/Observed/Analysis/1983-2012", "Calibration-flow-1-LT-Dam-monthly.csv")
      obs = read.csv(obsfile, header = T)
      obs = obs[c("yearmon", "sim_cms")]  # SWAT output using observed weather data
      #obs = obs[c("yearmon", "obs_cms")] # Real observation data
      obs = unique(obs)
      colnames(obs) = c("yearmon", "obs")

      syear = min(as.numeric(substr(dirlist, 1, 4))) + 1
      eyear = max(as.numeric(substr(dirlist, 1, 4)))

      cnt = 1
      for(m in syear:eyear){

        yearmon = sprintf("%4d-%02d", m, i)

        fimon = i - j
        if(fimon <= 0){
          fiyear = m - 1
          fimon = fimon + 12
          fiyearmon = sprintf("%4d-%02d", fiyear, fimon)
        } else {
          fiyear = m
          fiyearmon = sprintf("%4d-%02d", fiyear, fimon)
        }

        fcstfile = file.path(indir, fiyearmon, paste("Flow-", fiyearmon, "-", esm, "-1-monthly.csv", sep=""))
        fcst = read.csv(fcstfile, header = T)
        fcst = fcst[c("yearmon", "sim_cms")]
        fcst = unique(fcst)
        colnames(fcst) = c("yearmon", "fcst")


        obsdata = obs[which(obs$yearmon == yearmon), c("obs")]
        fcstdata = fcst[which(fcst$yearmon == yearmon), c("fcst")]

        yearmondata = cbind(yearmon, obsdata, fcstdata)
        if(cnt == 1){
          out = yearmondata
          cnt = cnt + 1
        } else {
          out = rbind(out, yearmondata)
        }
      }

      out = as.data.frame(out)

      colnames(out) = c("yearmon", "obs", "fcst")
      OutFName = file.path(outdir, sprintf("%s-Mon%02d-LT%02d_Monthly-SWAT.csv", esm, i, j))
      write.csv(out, OutFName, row.names = F)

      # Create contingency table for each month
      #obscat = cut(as.numeric(out$obs), c(-20, -2, -1.5, -1, 20), labels=c("ED", "SD", "ND", "NN"))
      #fcstcat = cut(as.numeric(out$fcst), c(-20, -2, -1.5, -1, 20), labels=c("ED", "SD", "ND", "NN"))
      #CTable = table(fcstcat, obscat)

      cor = format(cor(as.numeric(out$fcst), as.numeric(out$obs)), digits=2)
      nrmse = format(hydroGOF::nrmse(as.numeric(out$fcst), as.numeric(out$obs), norm="sd")/100, digits=2)

      #SScore = verification::multi.cont(CTable, baseline = NULL)
      #pc = format(SScore$pc, digits=2)
      #hss = format(SScore$hss, digits=2)

      if(j == 1) {
        cor_tbl = cbind(i, cor)
        nrmse_tbl = cbind(i, nrmse)
        #pc_tbl = cbind(i, pc)
        #hss_tble = cbind(i, hss)
      } else {
        cor_tbl = cbind(cor_tbl, cor)
        nrmse_tbl = cbind(nrmse_tbl, nrmse)
        #pc_tbl = cbind(pc_tbl, pc)
        #hss_tble = cbind(hss_tble, hss)
      }

    } # Lead time Loop
    colnames(cor_tbl) = c("Month", "LT1", "LT2", "LT3", "LT4", "LT5", "LT6")
    colnames(nrmse_tbl) = c("Month", "LT1", "LT2", "LT3", "LT4", "LT5", "LT6")
    #colnames(pc_tbl) = c("Month", "LT1", "LT2", "LT3", "LT4", "LT5", "LT6")
    #colnames(hss_tble) = c("Month", "LT1", "LT2", "LT3", "LT4", "LT5", "LT6")

    if(i == 1){
      cor_smry = cor_tbl
      nrmse_smry = nrmse_tbl
      #pc_smry = pc_tbl
      #hss_smry = hss_tble
    } else {
      cor_smry = rbind(cor_smry, cor_tbl)
      nrmse_smry = rbind(nrmse_smry,nrmse_tbl)
      #pc_smry = rbind(pc_smry,pc_tbl)
      #hss_smry = rbind(hss_smry, hss_tble)
    }

  } # Month Loop

  write.csv(cor_smry, file.path(outdir, sprintf("%s-Summary_TCC.csv", esm)), row.names = F)
  write.csv(nrmse_smry, file.path(outdir, sprintf("%s-Summary_NRMSE.csv", esm)), row.names = F)
  #write.csv(pc_smry, file.path(outdir, sprintf("%s-Summary_PC.csv", esm)), row.names = F)
  #write.csv(hss_smry, file.path(outdir, sprintf("%s-Summary_HSS.csv", esm)), row.names = F)
}



############### Calculating Forecasting Skill Score ########################
library(verification)
library(hydroGOF)
SwatFcstDir = EnvList$SwatFcstDir
esms = c("MME", "MIN", "MAX")
#indir = file.path(SwatFcstDir, "Output")
#outdir = file.path(SwatFcstDir, "Analysis")
indir = "H:/SWAT-Climate/CliWARA/SWAT/SForecast/Analysis"
outdir = indir
SetWorkingDir(outdir)

options(stringsAsFactors = FALSE)

for(k in 1:3){
  esm = esms[k]
  for(i in 1:12){
    for(j in 1:6){

      dirlist = list.dirs(indir, full.names = F)
      dirlist = dirlist[which(nchar(dirlist) > 0)]

      # Read Observed EDI
      syear = min(as.numeric(substr(dirlist, 1, 4))) + 1
      eyear = max(as.numeric(substr(dirlist, 1, 4))) - 1

      cnt = 1
      for(m in syear:eyear){

        yearmon = sprintf("%4d-%02d", m, i)

        fimon = i - j
        if(fimon <= 0){
          fiyear = m - 1
          fimon = fimon + 12
          fiyearmon = sprintf("%4d-%02d", fiyear, fimon)
        } else {
          fiyear = m
          fiyearmon = sprintf("%4d-%02d", fiyear, fimon)
        }

        # Need to add watershed outlet
        fcstfile = file.path(indir, fiyearmon, paste("Flow-", fiyearmon, "-", esm, "-1-monthly.csv", sep=""))
        fcst = read.csv(fcstfile, header = T)
        fcst = fcst[c("yearmon", "obs_cms", "sim_cms")]
        colnames(fcst) = c("yearmon", "obs", "fcst")

        fcstdata = fcst[which(fcst$yearmon == yearmon), ]

        if(cnt == 1){
          out = fcstdata
          cnt = cnt + 1
        } else {
          out = rbind(out, fcstdata)
        }
      }

      out = as.data.frame(out)

      colnames(out) = c("yearmon", "obs", "fcst")
      OutFName = file.path(outdir, sprintf("%s-Mon%02d-LT%02d_Monthly-SWAT.csv", esm, i, j))
      write.csv(out, OutFName, row.names = F)

      cor = format(cor(as.numeric(out$fcst), as.numeric(out$obs)), digits=2)
      nrmse = format(hydroGOF::nrmse(as.numeric(out$fcst), as.numeric(out$obs), norm="sd")/100, digits=2)

      if(j == 1) {
        cor_tbl = cbind(i, cor)
        nrmse_tbl = cbind(i, nrmse)
      } else {
        cor_tbl = cbind(cor_tbl, cor)
        nrmse_tbl = cbind(nrmse_tbl, nrmse)
      }

    } # Lead time Loop
    colnames(cor_tbl) = c("Month", "LT1", "LT2", "LT3", "LT4", "LT5", "LT6")
    colnames(nrmse_tbl) = c("Month", "LT1", "LT2", "LT3", "LT4", "LT5", "LT6")

    if(i == 1){
      cor_smry = cor_tbl
      nrmse_smry = nrmse_tbl
    } else {
      cor_smry = rbind(cor_smry, cor_tbl)
      nrmse_smry = rbind(nrmse_smry,nrmse_tbl)
    }

  } # Month Loop

  write.csv(cor_smry, file.path(outdir, sprintf("%s-Summary_TCC.csv", esm)), row.names = F)
  write.csv(nrmse_smry, file.path(outdir, sprintf("%s-Summary_NRMSE.csv", esm)), row.names = F)
  #write.csv(pc_smry, file.path(outdir, sprintf("%s-Summary_PC.csv", esm)), row.names = F)
  #write.csv(hss_smry, file.path(outdir, sprintf("%s-Summary_HSS.csv", esm)), row.names = F)
}



################### Calibration ##############################################
################# CN2.mgt change #############################################
srchstr = "CN2: Initial"
valposition = 16
valformat = "%16.2f"
pincrease = 0
valmax = 95
valmin = 0

indir = "E:/SCService-RM/CliWARA_SWAT/Scenarios/Default/TxtInOut"
simdir = "E:/SWAT-Climate/CliWARA/SWAT/TxtInOut"
flist = list.files(indir, pattern = glob2rx("*.mgt"), full.names=F)
fcnt = length(flist)
for(i in 1:fcnt){
  fname = flist[i]
  InDFile = paste(indir, "/", fname, sep="")
  OutDFile = paste(simdir, "/", fname, sep="")
  Swat.Percent.Change.Parameter(InDFile, OutDFile, srchstr,valposition, valformat, valmin, valmax, pincrease)
}


################# Manning n #############################################
srchstr = "CH_N2 : Manning"
valposition = 14
valformat = "%14.3f"
pincrease = 0
valmax = 0.014
valmin = 0

indir = "E:/SCService-RM/CliWARA_SWAT/Scenarios/Default/TxtInOut"
simdir = "E:/SWAT-Climate/CliWARA/SWAT/TxtInOut"
flist = list.files(indir, pattern = glob2rx("*.rte"), full.names=F)
fcnt = length(flist)
for(i in 1:fcnt){
  fname = flist[i]
  InDFile = paste(indir, "/", fname, sep="")
  OutDFile = paste(simdir, "/", fname, sep="")
  Swat.Percent.Change.Parameter(InDFile, OutDFile, srchstr,valposition, valformat, valmin, valmax, pincrease)
}

################# Manning n #############################################
srchstr = "OV_N : Manning"
valposition = 16
valformat = "%16.3f"
pincrease = 0
valmax = 0.140
valmin = 0

indir = "E:/SCService-RM/CliWARA_SWAT/Scenarios/Default/TxtInOut"
simdir = "E:/SWAT-Climate/CliWARA/SWAT/TxtInOut"
flist = list.files(indir, pattern = glob2rx("*.hru"), full.names=F)
fcnt = length(flist)
for(i in 1:fcnt){
  fname = flist[i]
  InDFile = paste(indir, "/", fname, sep="")
  OutDFile = paste(simdir, "/", fname, sep="")
  Swat.Percent.Change.Parameter(InDFile, OutDFile, srchstr,valposition, valformat, valmin, valmax, pincrease)
}




