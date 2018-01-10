rm(list=ls())
library(png)
library(Hmisc)

source("D:/Dropbox/R-packages/rswat/R/util.R")
source("D:/Dropbox/R-packages/rswat/R/calibration.R")
source("D:/Dropbox/R-packages/rswat/R/cchange_swat.R")
source("D:/Dropbox/R-packages/rswat/R/sforecast_swat.R")
source("D:/Dropbox/R-packages/rswat/R/input_swat.R")
source("D:/Dropbox/R-packages/rswat/R/output_swat.R")
source("D:/Dropbox/R-packages/rswat/R/graph_swat.R")

# Setting working environment
#EnvList = Set.Working.Environment (BaseDir="F:/Drought", PrjName = "Korea")
EnvList = Set.Working.Environment (BaseDir="F:/SWAT-Climate", PrjName = "Choongjoo")

################# SWAT Run Using Observed data #####################################
## Calibration
#Swat.Observation.Run (EnvList, SimType="Calibration", Syear=2002, Eyear=2009)
#Swat.Observation.Rch.Analysis (EnvList, SimType="Calibration", Syear=2002)
## Validation
#Swat.Observation.Run (EnvList, SimType="Validation", Syear=2008, Eyear=2012)
#Swat.Observation.Rch.Analysis (EnvList, SimType="Validation", Syear=2008)
## Forecast
#Swat.Observation.Run (EnvList, SimType="Forecasts", Syear=2011, Eyear=2016)
#Swat.Observation.Rch.Analysis (EnvList, SimType="Forecasts", Syear=2011)
## Forecast
Swat.Observation.Run (EnvList, SimType="Climatology", Syear=1984, Eyear=2013)
Swat.Observation.Rch.Analysis (EnvList, SimType="Forecasts", Syear=1984)


################# SWAT Run Using using Climate Change ##############################
#Swat.CChange.Run (EnvList)

################################library(sfsmisc)
# Read necessary information

  library(dplyr)
  library(stringr)
  fiyearmon = "2015-09"

  SwatFcstDir = EnvList$SwatFcstDir
  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  SwatRunDir = EnvList$SwatRunDir
  StnIDs = EnvList$StnIDs
  NYSKIP = as.numeric(EnvList$CioNYSKIP)
  #NYSKIP = 0
  FcstDataDir = EnvList$FcstDataDir
  ObsDayDir = EnvList$ObsDayDir

  StnIDs = c("ID100", "ID114", "ID221", "ID221", "ID127")

  Syear = as.numeric(substr(fiyearmon, 1, 4)) - 1


  FcstDir = file.path(FcstDataDir, fiyearmon)
  InDirs = list.dirs(FcstDir)
  InDirs = InDirs[2:length(InDirs)]

  outdir = file.path(SwatFcstDir, "Output", fiyearmon)
  SetWorkingDir(outdir)

  for(i in 1:length(InDirs)){
    InDir = InDirs[i]

    indirnm = str_split(InDir, "/")
    indirnm = indirnm[[1]][length(indirnm[[1]])]

    #outdir = file.path(SwatFcstDir, "Output", fiyearmon, indirnm)
    #SetWorkingDir(outdir)

    for(j in 1:length(StnIDs)){

      StnID = StnIDs[j]

      ObsDFname = list.files(ObsDayDir, pattern = glob2rx(paste(StnID, "*.*", sep="")), full.names = T)
      obsdata = read.csv(ObsDFname, header = T)
      colnames(obsdata) = c("Year", "Mon", "Day", "prcp", "tmax", "tmin", "wspd", "rhum", "rsds", "sshine", "cloud", "tavg")
      srow = which(obsdata$Year == (Syear-NYSKIP) & obsdata$Mon == 1 & obsdata$Day == 1)
      erow = which(obsdata$Year == as.numeric(substr(fiyearmon, 1, 4)) & obsdata$Mon == as.numeric(substr(fiyearmon, 6, 7)) & obsdata$Day == 1) - 1
      obsdata = obsdata[srow:erow, ]

      FcstDFname = list.files(InDir, paste(StnID, ".csv", sep=""), full.names = T)
      fcstdata = read.csv(FcstDFname, header = T)
      fcstdata$Year = substr(fcstdata$date, 1, 4)
      fcstdata$Mon = substr(fcstdata$date, 6, 7)
      fcstdata$Day = substr(fcstdata$date, 9, 10)
      fcstdata = fcstdata[c("Year", "Mon", "Day", "prcp", "tmax", "tmin", "wspd", "rhum", "rsds", "sshine", "cloud", "tavg")]

      emon = as.numeric(fcstdata$Mon[nrow(fcstdata)])
      eyear = as.numeric(fcstdata$Year[nrow(fcstdata)])
      if(emon < 12){
        dumydata = as.data.frame(seq(as.Date(sprintf("%d-%02d-01", eyear, emon+1)), as.Date(sprintf("%d-12-31", eyear)), by="day"))
        colnames(dumydata) = "date"
        dumydata$Year = substr(dumydata$date, 1, 4)
        dumydata$Mon = substr(dumydata$date, 6, 7)
        dumydata$Day = substr(dumydata$date, 9, 10)
        dumydata$prcp = -99.0
        dumydata$tmax = -99.0
        dumydata$tmin = -99.0
        dumydata$wspd = -99.0
        dumydata$rhum = -99.0
        dumydata$rsds = -99.0
        dumydata$sshine = -99.0
        dumydata$cloud = -99.0
        dumydata$tavg = -99.0
        dumydata = dumydata[c("Year", "Mon", "Day", "prcp", "tmax", "tmin", "wspd", "rhum", "rsds", "sshine", "cloud", "tavg")]
      }


      outdata = rbind(obsdata, fcstdata, dumydata)
      #outdata = rbind(obsdata, fcstdata)
      write.csv(outdata, paste(outdir, "/", StnID, ".csv", sep=""), row.names = F)

    }

    # Create SWAT Weather Input files using observed data
    Swat.Write.Weather.Input.MultiStations (StnDir, StnFile, StnIDs, outdir, SwatRunDir)

    Eyear = max(as.numeric(outdata$Year))
    NBYR = Eyear - Syear + 1 + NYSKIP
    IYR = Syear - NYSKIP
    Write.Cio.Input (SwatRunDir, NBYR, IYR, NYSKIP)

    # Run SWAT model ###########################
    setwd(SwatRunDir)
    system("cmd.exe /c SWAT2005.exe")

    ## Rename the output.files
    OutputTypes = EnvList$OutputTypes
    for(k in 1:length(OutputTypes)){
      FromDFile = paste(SwatRunDir, "/output.",  OutputTypes[k], sep="")
      ToDFile = paste(outdir, "/output-", fiyearmon, "-", indirnm, ".", OutputTypes[k], sep="")
      file.rename(FromDFile, ToDFile)
    }


    #NYSKIP = EnvList$CioNYSKIP
    rchfile = paste("output-", fiyearmon, "-", indirnm, ".rch", sep="")
    sdate = sprintf("%4s-01-01",Syear)
    sdate2 = sprintf("%4s-01-01", as.numeric(Syear) + as.numeric(NYSKIP))
    outlet = EnvList$OutletIDs

    outtype = "water"
    funtype = "mean"
    #rch = Swat.Rch.Summary (outdir, rchfile, outlet, sdate, outtype, funtype, sdate2)
    rch = Swat.Rch.Summary (outdir, rchfile, outlet, sdate, outtype, funtype)
    sim = rch$data[, c("date", "flow_cms")]
    colnames(sim) = c("date", "sim_cms")


    SwatDbDir = EnvList$SwatDbDir
    ObsFile = EnvList$ObsFile
    obs = read.csv(file.path(SwatDbDir, ObsFile))
    obs$date = as.Date(sprintf("%4d-%02d-%02d", obs$year, obs$mon, obs$day))
    obs = obs[, c("date", "inflow_cms")]
    colnames(obs) = c("date", "obs_cms")

    data = merge(obs, sim, all=T)
    data = na.omit(data)

    imsi = Obs.Fill.Date(data)
    mdata = aggregate(cbind(obs_cms, sim_cms) ~ yearmon, data=imsi, FUN=mean)
    FcstDFname = list.files(InDir, paste(StnID, ".csv", sep=""), full.names = T)
    fcstdata = read.csv(FcstDFname, header = T)
    fcstdata$Year = substr(fcstdata$date, 1, 4)
    fcstdata$Mon = substr(fcstdata$date, 6, 7)
    emon = as.numeric(fcstdata$Mon[nrow(fcstdata)])
    eyear = as.numeric(fcstdata$Year[nrow(fcstdata)])
    erow = which(as.numeric(substr(mdata$yearmon, 1, 4)) == eyear & as.numeric(substr(mdata$yearmon, 6, 7)) == emon)
    mdata = mdata[1:erow, ]


    #outname = paste("Flow-", fiyearmon, "-", indirnm, "-", outlet, ".csv",sep="")
    #write.csv(data, outname, row.names = F)

    outname = paste("Flow-", fiyearmon, "-", indirnm, "-", outlet, "-monthly.csv", sep="")
    write.csv(mdata, outname, row.names = F)

  }






