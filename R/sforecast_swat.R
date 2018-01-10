################################library(sfsmisc)
# Read necessary information
Swat.SForecast.Run <- function(EnvList, fiyearmon) {

  library(dplyr)
  library(stringr)

  SwatFcstDir = EnvList$SwatFcstDir
  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  SwatRunDir = EnvList$SwatRunDir
  StnIDs = EnvList$StnIDs
  fiyearmode = EnvList$fiyearmode
  NYSKIP = as.numeric(EnvList$CioNYSKIP)
  FcstDataDir = EnvList$FcstDataDir
  ObsDayDir = EnvList$ObsDayDir

  StnIDs = EnvList$StnIDs

  Syear = as.numeric(substr(fiyearmon, 1, 4)) - 3

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
      if(fiyearmode == T){
        erow = which(obsdata$Year == as.numeric(substr(fiyearmon, 1, 4)) & obsdata$Mon == as.numeric(substr(fiyearmon, 6, 7)) & obsdata$Day == 1) - 1
      } else {
        if(as.numeric(substr(fiyearmon, 6, 7)) < 12){
          erow = which(obsdata$Year == as.numeric(substr(fiyearmon, 1, 4)) & obsdata$Mon == (as.numeric(substr(fiyearmon, 6, 7)) + 1) & obsdata$Day == 1) - 1
        } else {
          erow = which(obsdata$Year == (as.numeric(substr(fiyearmon, 1, 4)) + 1) & obsdata$Mon == 1 & obsdata$Day == 1) - 1
        }
      }
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
        outdata = rbind(obsdata, fcstdata, dumydata)
      } else {
        outdata = rbind(obsdata, fcstdata)
      }


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


  }

}

##################################################
Swat.SForecast.Rch.Analysis <- function(EnvList, fiyearmon) {

  library(stringr)

  SwatFcstDir = EnvList$SwatFcstDir
  # StnDir = EnvList$StnDir
  # StnFile = EnvList$StnFile
  # SwatRunDir = EnvList$SwatRunDir
  # StnIDs = EnvList$StnIDs
  # fiyearmode = EnvList$fiyearmode
  NYSKIP = as.numeric(EnvList$CioNYSKIP)
  # #NYSKIP = 0
  FcstDataDir = EnvList$FcstDataDir
  # ObsDayDir = EnvList$ObsDayDir
  #
  #
  Syear = as.numeric(substr(fiyearmon, 1, 4)) - 1


  FcstDir = file.path(FcstDataDir, fiyearmon)
  InDirs = list.dirs(FcstDir)
  InDirs = InDirs[2:length(InDirs)]

  indir = file.path(SwatFcstDir, "Output", fiyearmon)
  SetWorkingDir(indir)

  for(i in 1:length(InDirs)){

    InDir = InDirs[i]

    indirnm = str_split(InDir, "/")
    indirnm = indirnm[[1]][length(indirnm[[1]])]

    #NYSKIP = EnvList$CioNYSKIP
    rchfile = paste("output-", fiyearmon, "-", indirnm, ".rch", sep="")
    sdate = sprintf("%4s-01-01",Syear)
    sdate2 = sprintf("%4s-01-01", as.numeric(Syear) + as.numeric(NYSKIP))
    outlets = EnvList$OutletIDs

    outtype = "water"
    funtype = "mean"

    for(j in 1:length(outlets)){

      outlet = outlets[j]

      rch = Swat.Rch.Summary (indir, rchfile, outlet, sdate, outtype, funtype)
      sim = rch$data[, c("date", "flow_cms")]
      colnames(sim) = c("date", "sim_cms")

      SwatDbDir = EnvList$SwatDbDir
      ObsFile = EnvList$ObsFile
      obs = read.csv(file.path(SwatDbDir, ObsFile))

      ## Daily Observed data
      if("day" %in% colnames(obs)){

        sim = rch$data[, c("date", "flow_cms")]
        colnames(sim) = c("date", "sim_cms")

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

        #outletnm = EnvList$OutletNms

        outdir = file.path(SwatFcstDir, "Analysis", fiyearmon)
        SetWorkingDir(outdir)

        #outname = paste("Flow-", fiyearmon, "-", indirnm, "-", outlet, "-daily.csv", sep="")
        #write.csv(data, outname, row.names = F)

        outname = paste("Flow-", fiyearmon, "-", indirnm, "-", outlet, "-monthly.csv", sep="")
        write.csv(mdata, outname, row.names = F)

      } else {

        sim_mon = rch$ymdata[, c("yearmon", "flow_cms")]
        colnames(sim_mon) = c("yearmon", "sim_cms")

        obs$yearmon = sprintf("%4d-%02d", obs$year, obs$mon)
        obs_mon = obs[, c("yearmon", "inflow_cms")]
        colnames(obs_mon) = c("yearmon", "obs_cms")

        mdata = merge(obs_mon, sim_mon, all=T)
        mdata = na.omit(mdata)

        #outletnm = EnvList$OutletNms
        StnIDs = EnvList$StnIDs
        StnID = StnIDs[1]

        FcstDFname = list.files(InDir, paste(StnID, ".csv", sep=""), full.names = T)
        fcstdata = read.csv(FcstDFname, header = T)
        fcstdata$Year = substr(fcstdata$date, 1, 4)
        fcstdata$Mon = substr(fcstdata$date, 6, 7)
        emon = as.numeric(fcstdata$Mon[nrow(fcstdata)])
        eyear = as.numeric(fcstdata$Year[nrow(fcstdata)])
        erow = which(as.numeric(substr(mdata$yearmon, 1, 4)) == eyear & as.numeric(substr(mdata$yearmon, 6, 7)) == emon)
        mdata = mdata[1:erow, ]

        outdir = file.path(SwatFcstDir, "Analysis", fiyearmon)
        SetWorkingDir(outdir)

        outname = paste("Flow-", fiyearmon, "-", indirnm, "-", outlet, "-monthly.csv", sep="")
        write.csv(mdata, outname, row.names = F)

      }

    }

  }

}




