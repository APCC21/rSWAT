#' @export
Set.Working.Environment <- function(envfile, override=list()) {

  options(stringsAsFactors=FALSE)

  data <- yaml::yaml.load_file(envfile)

  data <- lapply(data, function(x) if (is.character(x)) gsubfn::gsubfn("\\$\\((.*?)\\)", data, x) else x)

  if(data$StnDir == "User") data$StnDir = data$ObsDayDir
  if(data$StnDir == "GHCN") data$StnDir = data$GhcnDir

  if(!file.exists(data$ObsDayDir)) dir.create(data$ObsDayDir, showWarnings=F,recursive=T)
  if(!file.exists(data$GhcnDir)) dir.create(data$GhcnDir, showWarnings=F,recursive=T)
  if(!file.exists(data$BndDir)) dir.create(data$BndDir, showWarnings=F,recursive=T)
  if(!file.exists(data$GhcnDir)) dir.create(data$GhcnDir, showWarnings=F,recursive=T)
  if(!file.exists(data$SmplDir)) dir.create(data$SmplDir, showWarnings=F,recursive=T)
  if(!file.exists(data$SwatDbDir)) dir.create(data$SwatDbDir, showWarnings=F,recursive=T)
  if(!file.exists(data$SwatRunDir)) dir.create(data$SwatRunDir, showWarnings=F,recursive=T)
  if(!file.exists(data$SwatObsDir)) dir.create(data$SwatObsDir, showWarnings=F,recursive=T)
  if(!file.exists(data$SwatCcDir)) dir.create(data$SwatCcDir, showWarnings=F,recursive=T)
  if(!file.exists(data$SwatFcstDir)) dir.create(data$SwatFcstDir, showWarnings=F,recursive=T)

  outList = list("PrjDir" = data$PrjDir,
                 "CcDataDir" = data$CcDataDir,
                 "FcstDataDir" = data$FcstDataDir,
                 "StnDir" = data$StnDir,
                 "ObsDayDir" = data$ObsDayDir,
                 "GhcnDir" = data$GhcnDir,
                 "SmplDir" = data$SmplDir,
                 "SwatRunDir" = data$SwatRunDir,
                 "SwatObsDir" = data$SwatObsDir,
                 "SwatCcDir" = data$SwatCcDir,
                 "SwatFcstDir" = data$SwatFcstDir,
                 "SwatDbDir" = data$SwatDbDir,
                 "CioNYSKIP" = data$CioNYSKIP,
                 "Syear_cal" = data$Syear_cal,
                 "Eyear_cal" = data$Eyear_cal,
                 "Syear_val" = data$Syear_val,
                 "Eyear_val" = data$Eyear_val,
                 "Syear_clim" = data$Syear_clim,
                 "Eyear_clim" = data$Eyear_clim,
                 "StnFile" = data$StnFile,
                 "StnIDs" = data$StnIDs,
                 "OutletIDs" = data$OutletIDs,
                 "OutletNms" = data$OutletNms,
                 "ObsFile" = data$ObsFile,
                 "CChangeOpt" = data$CChangeOpt,
                 "MdlNms" = data$MdlNms,
                 "ScnNms" = data$ScnNms,
                 "Syear_hist" = data$Syear_hist,
                 "Eyear_hist" = data$Eyear_hist,
                 "Syear_rcp" = data$Syear_rcp,
                 "Eyear_rcp" = data$Eyear_rcp,
                 "SForecastOpt" = data$SForecastOpt,
                 "fiyearmode" = data$fiyearmode,
                 "Syear_Fcst" = data$Syear_Fcst,
                 "Eyear_Fcst" = data$Eyear_Fcst,
                 "OutputTypes" = data$OutputTypes)

  # override
  for (varname in names(override)) {
    outList[[varname]] <- override[[varname]]
  }
  return(outList)

}

################################################################################
# Add year, month, yearmon column
################################################################################
#' @export
SetWorkingDir <- function(wdir) {

  # Creat working dir if not exists
  dir.create(wdir, showWarnings=F,recursive=T)
  setwd(wdir)

  return(wdir)

}

################################################################################
# Add year, month, yearmon column
################################################################################
Obs.Fill.Date <- function(x, sdate=NULL, edate=NULL) {

  if(!missing(sdate) & !missing(edate)){
    sdate = as.Date(sdate)
    edate = as.Date(edate)
    imsi = as.data.frame(seq(sdate, edate, by=1))
    colnames(imsi) = "date"

    x = x[which(x[,1]>=sdate & x[,1]<=edate),]

    data = merge(imsi, x, all=T)

    year=as.numeric (format(data[,1],"%Y"))
    month=as.numeric (format(data[,1],"%m"))
    yearmon=as.character (format(data[,1],"%Y-%m"))
    data <- cbind(year, month, yearmon, data)
  }

  if(!missing(sdate) & missing(edate)){
    sdate = as.Date(sdate)
    nrows = length(x[,1])
    date = seq(sdate, sdate+nrows-1, by=1)

    data = cbind.data.frame(date, x)

    year=as.numeric (format(data[,1],"%Y"))
    month=as.numeric (format(data[,1],"%m"))
    yearmon=as.character (format(data[,1],"%Y-%m"))
    data <- cbind(year, month, yearmon, data)

  }

  if(missing(sdate) & missing(edate)){
    x[,1]=as.Date(x[,1])
    year=as.numeric (format(x[,1],"%Y"))
    month=as.numeric (format(x[,1],"%m"))
    yearmon=as.character (format(x[,1],"%Y-%m"))
    data <- cbind(year, month, yearmon, x)
  }

  return(data)

}

###################################################################################################################
# Copy necessary input files into simulation folder
###################################################################################################################
Swat.Copy.Input.Output.Files <- function(comdir, inputdir, scndir, swatdir, type) {


  if(type == "Input") {

    # Copy common input files
    fnames = c("swat2009-488.exe", "swat2009-528.exe", "swat2009-591.exe", "basins.bsn")
    filecnt = length(fnames)
    for(i in 1:filecnt){
      common = paste(comdir, "/",  fnames[i], sep="")
      swat = paste(swatdir, "/",  fnames[i], sep="")
      file.copy(common, swat, overwrite=T)
    }


    # Copy swat input files
    setwd(inputdir)
    fnames = list.files(inputdir, pattern = glob2rx("*.*"), full.names=F)
    filecnt = length(fnames)
    for(i in 1:filecnt){
      input = paste(inputdir, "/",  fnames[i], sep="")
      swat = paste(swatdir, "/",  fnames[i], sep="")
      file.copy(input, swat, overwrite=T)
    }

    # Copy weather input files
    fnames = c("pcp1.pcp", "tmp1.tmp", "hmd.hmd", "wnd.wnd", "slr.slr")
    filecnt = length(fnames)
    for(i in 1:filecnt){
      scenario = paste(scndir, "/",  fnames[i], sep="")
      swat = paste(swatdir, "/",  fnames[i], sep="")
      file.copy(scenario, swat, overwrite=T)
    }
  }

  if(type == "Output") {
    # Copy weather input files
    fnames = c("output.hru", "output.sub", "output.rch", "output.wtr", "output.std", "output.sed", "output.rsv")
    filecnt = length(fnames)
    for(i in 1:filecnt){
      scenario = paste(scndir, "/",  fnames[i], sep="")
      swat = paste(swatdir, "/",  fnames[i], sep="")
      file.copy(swat, scenario, overwrite=T)
    }
  }


}

