################################################################################
#SimType="Calibration"; Syear = 2005
Swat.Observation.Rch.Analysis <- function(EnvList, SimType, Syear) {

  NYSKIP = EnvList$CioNYSKIP
  wdir = file.path(EnvList$SwatObsDir, "Output")
  rchfile = paste("output-", SimType, ".rch", sep="")
  sdate = sprintf("%4s-01-01", Syear)
  sdate2 = sprintf("%4s-01-01", Syear + as.numeric(NYSKIP))
  outlets = EnvList$OutletIDs

  for(i in 1:length(outlets)){
    outlet = outlets[i]
    outtype = "water"
    funtype = "mean"
    rch = Swat.Rch.Summary (wdir, rchfile, outlet, sdate, outtype, funtype, sdate2)

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

      outletnm = EnvList$OutletNms

      pngname = paste(SimType, "-flow-", outlet, "-", outletnm, sep="")
      maintitle = pngname

      outdir = file.path(EnvList$SwatObsDir, "Analysis")
      SetWorkingDir(outdir)
      results = Swat.Summary.Graph.Table(outdir, pngname, maintitle, data, mdata)

    } else {

      data = NULL

      sim_mon = rch$ymdata[, c("yearmon", "flow_cms")]
      colnames(sim_mon) = c("yearmon", "sim_cms")

      #sim$yearmon = substr(sim$date, 1, 7)
      #sim_mon = aggregate(cbind(sim_cms) ~ yearmon, data=sim, FUN=mean)

      obs$yearmon = sprintf("%4d-%02d", obs$year, obs$mon)
      obs_mon = obs[, c("yearmon", "inflow_cms")]
      colnames(obs_mon) = c("yearmon", "obs_cms")

      mdata = merge(obs_mon, sim_mon, all=T)
      mdata = na.omit(mdata)

      outletnm = EnvList$OutletNms

      pngname = paste(SimType, "-flow-", outlet, "-", outletnm, sep="")
      maintitle = pngname

      outdir = file.path(EnvList$SwatObsDir, "Analysis")
      SetWorkingDir(outdir)
      results = Swat.Summary.Graph.Table(outdir, pngname, maintitle, data, mdata)

    }


  }


}


################################################################################
Swat.Observation.Run <- function(EnvList, SimType, Syear, Eyear) {

  ObsDayDir = EnvList$ObsDayDir
  SwatRunDir = EnvList$SwatRunDir
  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  StnIDs = EnvList$StnIDs
  NYSKIP = as.numeric(EnvList$CioNYSKIP)
  SwatObsDir = EnvList$SwatObsDir

  outdir = file.path(SwatObsDir, "Output")
  SetWorkingDir(outdir)

  # Create SWAT Weather Input files using observed data
  Swat.Write.Weather.Input.MultiStations (StnDir, StnFile, StnIDs, ObsDayDir, SwatRunDir)

  # Update file.cio input file
  NBYR = Eyear - Syear + 1 + NYSKIP
  IYR = Syear - NYSKIP
  Write.Cio.Input (SwatRunDir, NBYR, IYR, NYSKIP)

  # Copy SWAT2005.exe ###########################
  SwatExe <- system.file("extdata", "SWAT2005.exe", package = "rswat")
  if(!file.exists(file.path(SwatRunDir, "SWAT2005.exe"))) { file.copy(SwatExe, SwatRunDir) }

  # Run SWAT model ###########################
  setwd(SwatRunDir)
  system("cmd.exe /c SWAT2005.exe")

  ## Rename the output.files
  OutputTypes = EnvList$OutputTypes
  for(k in 1:length(OutputTypes)){
    FromDFile = paste(SwatRunDir, "/output.",  OutputTypes[k], sep="")
    ToDFile = paste(outdir, "/output-", SimType, ".", OutputTypes[k], sep="")
    file.rename(FromDFile, ToDFile)
  }

}


################################################################################
Swat.Percent.Change.Parameter <- function(InDFile, OutDFile, srchstr,valposition, valformat, valmin, valmax, pincrease) {

  #   # One for test
  #   InDFile = "F:/SWAT-NakDong/2001/Scenarios/Default/CalInOut/test.mgt"
  #   OutDFile = "F:/SWAT-NakDong/2001/Scenarios/Default/CalInOut/test-out.mgt"
  #   srchstr = "CN2:"
  #   valposition = 16
  #   valformat = "%16.2f"
  #   pincrease = 10

  con = file(InDFile, "r")
  sink(OutDFile)
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {

    if(grepl(srchstr, oneLine)){
      val = as.numeric(substr(oneLine,1,valposition))
      newval = val * (1+pincrease/100)
      if(newval >= valmax){newval = valmax}
      if(newval <= valmin){newval = valmin}
      str = substr(oneLine,valposition+1,nchar(oneLine))
      newline = sprintf(paste(valformat, "%s", sep=""),newval, str)
      cat(newline, sep="\n")
    } else {
      cat(oneLine, sep="\n")
    }
  }

  close(con)
  sink()


}

################################################################################
Swat.Fixed.Change.Parameter <- function(InDFile, OutDFile, srchstr,valposition, valformat, fixedval) {

  #   # One for test
  #   InDFile = "F:/SWAT-NakDong/2001/Scenarios/Default/CalInOut/test.mgt"
  #   OutDFile = "F:/SWAT-NakDong/2001/Scenarios/Default/CalInOut/test-out.mgt"
  #   srchstr = "CN2:"
  #   valposition = 16
  #   valformat = "%16.2f"
  #   pincrease = 10

  con = file(InDFile, "r")
  sink(OutDFile)
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {

    if(grepl(srchstr, oneLine)){
      str = substr(oneLine,valposition+1,nchar(oneLine))
      newline = sprintf(paste(valformat, "%s", sep=""),fixedval, str)
      cat(newline, sep="\n")
    } else {
      cat(oneLine, sep="\n")
    }
  }

  close(con)
  sink()


}

################################################################################
Swat.Percent.Change.Soil.Parameter <- function(InDFile, OutDFile, srchstr, pincrease) {

  # One for test
  #   InDFile = "F:/SWAT-NakDong/2001/Scenarios/Default/CalInOut/000080001.sol"
  #   OutDFile = "F:/SWAT-NakDong/2001/Scenarios/Default/CalInOut/test-out.sol"
  #   srchstr = " Ksat"
  #   pincrease = 10

  valposition = 27


  con = file(InDFile, "r")
  sink(OutDFile)
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {

    if(grepl(srchstr, oneLine)){
      str = substr(oneLine,1,valposition)
      valstr = substr(oneLine,valposition+1,nchar(oneLine))
      valstr = sub("^\\s+", "", valstr)
      valstr = gsub("     ", ",", valstr)
      vals = as.numeric(unlist(strsplit(valstr, ",")))
      valcnt = length(vals)
      vals = vals * (1+pincrease/100)
      if(valcnt == 1) { newline = sprintf("%s%12.2f",str, vals[1]) }
      if(valcnt == 2) { newline = sprintf("%s%12.2f%12.2f",str, vals[1], vals[2]) }
      if(valcnt == 3) { newline = sprintf("%s%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3]) }
      if(valcnt == 4) { newline = sprintf("%s%12.2f%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3], vals[4]) }
      if(valcnt == 5) { newline = sprintf("%s%12.2f%12.2f%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3], vals[4], vals[5]) }
      if(valcnt == 6) { newline = sprintf("%s%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]) }
      if(valcnt == 7) { newline = sprintf("%s%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]) }
      if(valcnt == 8) { newline = sprintf("%s%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7], vals[8]) }
      if(valcnt == 9) { newline = sprintf("%s%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7], vals[8], vals[9]) }
      if(valcnt == 10) { newline = sprintf("%s%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f%12.2f",str, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7], vals[8], vals[9], vals[10]) }
      cat(newline, sep="\n")
    } else {
      cat(oneLine, sep="\n")
    }
  }

  close(con)
  sink()


}

