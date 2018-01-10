Swat.CChange.Run <- function(EnvList) {

  ScnNms = EnvList$ScnNms
  MdlNms = EnvList$MdlNms
  CcDataDir = EnvList$CcDataDir
  SwatRunDir = EnvList$SwatRunDir
  SwatCcDir = EnvList$SwatCcDir
  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  StnIDs = EnvList$StnIDs
  NYSKIP = as.numeric(EnvList$CioNYSKIP)


  for(i in 1:length(MdlNms)){

    mdlnm = MdlNms[i]

    outdir = file.path(SwatCcDir, "Output", mdlnm)
    SetWorkingDir(outdir)

    for(j in 1:length(ScnNms)){
      scnnm = ScnNms[j]

      # Create SWAT Weather Input files using observed data
      WthrDir = file.path(CcDataDir, mdlnm)
      Swat.Write.Weather.Input.MultiStations (StnDir, StnFile, StnIDs, WthrDir, SwatRunDir, scnnm)

      if(scnnm == "historical"){
        Syear = as.numeric(EnvList$Syear_hist)
        Eyear = as.numeric(EnvList$Eyear_hist)
      } else {
        Syear = as.numeric(EnvList$Syear_rcp)
        Eyear = as.numeric(EnvList$Eyear_rcp)
      }

      # Update file.cio input file
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
        ToDFile = paste(outdir, "/output-", scnnm, ".", OutputTypes[k], sep="")
        file.rename(FromDFile, ToDFile)
      }


    } # Scenario Loop
  } # Model Loop

}

Swat.CChange.Rch.Analysis <- function(EnvList) {


  ScnNms = EnvList$ScnNms
  MdlNms = EnvList$MdlNms
  SwatCcDir = EnvList$SwatCcDir
  NYSKIP = as.numeric(EnvList$CioNYSKIP)


  for(i in 1:length(MdlNms)){

    mdlnm = MdlNms[i]

    wdir = file.path(SwatCcDir, "Output", mdlnm)

    for(j in 1:length(ScnNms)){
      scnnm = ScnNms[j]

      if(scnnm == "historical"){
        Syear = as.numeric(EnvList$Syear_hist)
        Eyear = as.numeric(EnvList$Eyear_hist)
      } else {
        Syear = as.numeric(EnvList$Syear_rcp)
        Eyear = as.numeric(EnvList$Eyear_rcp)
      }

      rchfile = paste("output-", scnnm, ".rch", sep="")
      sdate = sprintf("%4s-01-01", Syear)
      sdate2 = sprintf("%4s-01-01", Syear + as.numeric(NYSKIP))
      outlets = EnvList$OutletIDs

      for(k in 1:length(outlets)){
        outlet = outlets[k]
        outtype = "water"
        funtype = "mean"
        rch = Swat.Rch.Summary (wdir, rchfile, outlet, sdate, outtype, funtype, sdate2)
        sim = rch$data[, c("date", "flow_cms")]
        colnames(sim) = c("date", "sim_cms")

        data = na.omit(sim)

        imsi = Obs.Fill.Date(data)
        mdata = aggregate(cbind(sim_cms) ~ yearmon, data=imsi, FUN=mean)

        outdir = file.path(SwatCcDir, "Analysis", mdlnm)
        SetWorkingDir(outdir)

        dayfilename = paste("output-", scnnm, "-", outlet, "-daily.csv", sep="")
        monfilename = paste("output-", scnnm, "-", outlet,  "-monthly.csv", sep="")

        write.csv(sim, dayfilename, row.names=F)
        write.csv(mdata, monfilename, row.names=F)


      }


    }

  }


}
