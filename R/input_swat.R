###################################################################################################################
# write weather input file for SWAT simulation
###################################################################################################################
Swat.Write.Weather.Input.MultiStations <- function(StnDir, StnFile, StnIDs, wthdir, outdir, fnamestr=NULL){

  # StnDir = EnvList$StnDir
  # StnFile = EnvList$StnFile
  # StnIDs = EnvList$StnIDs
  # wthdir = EnvList$ObsDayDir
  # outdir = EnvList$SwatRunDir
  # wthdir = file.path(CcDataDir, mdlnm)
  # fnamestr = "rcp45"

  library(zoo)

  stncnt = length(StnIDs)

  #### Header Information for SWAt
  stninfo = read.csv(paste(StnDir, "/", StnFile, sep=""), header=T)
  for(i in 1:stncnt){
    stnid = StnIDs[i]
    if(i == 1){
      data = stninfo[which(stninfo$ID ==  stnid),]
    } else {
      temp = stninfo[which(stninfo$ID ==  stnid),]
      data = rbind(data, temp)
    }
  }
  Longs = data$Lon; Latis = data$Lat; Elevs = data$Elev


  #outdir = paste(wthdir, "/", mdlnm, "/", scnnm, sep="")
  SetWorkingDir(outdir)

  ###### ?µ??? ��???? ?????? ?????? ####################
  varnms = c("prcp", "wspd", "rhum", "rsds")
  varcnt = length(varnms)
  for(k in 1:varcnt){
    varnm = varnms[k]


    for(ii in 1:stncnt){

      stnid = StnIDs[ii]

      if(!missing(fnamestr)){
        srchstr = paste("*", stnid, "*", fnamestr, ".csv", sep="")
      } else {
        srchstr = paste("*", stnid, "*.csv", sep="")
      }
      #srchstr = paste("*", stnid, "*", fnamestr, "*.csv", sep="")
      InDFile = list.files(wthdir, pattern = glob2rx(srchstr), full.names=T)
      data = read.csv(InDFile, header=T, na.strings = c("-99.00", "-99.0", "-99"))
      if(ncol(data) == 12) {
        colnames(data) = c("year", "mon", "day", "prcp","tmax","tmin","wspd","rhum","rsds", "sshine", "cloud", "tavg")
        data$date = as.Date(sprintf("%d-%02d-%02d", data$year, data$mon, data$day))
        data = data[c("date", "prcp","tmax","tmin","wspd","rhum","rsds")]
      } else {
        data$date = as.Date(sprintf("%d-%02d-%02d", data$year, data$mon, data$day))
        data = data[c("date", "prcp","tmax","tmin","wspd","rhum","rsds")]
      }

      if(ii == 1){
        coln = match(varnm, names(data))
        vardata = data[,c(1,coln)]
        colnames(vardata) = c("date", stnid)
        # missing, interpolate
        if(!all(is.na(vardata[2]))){vardata[, 2] = na.approx(vardata[, 2], na.rm=F)}
        vardata$date = as.Date(vardata$date)
      } else {
        coln = match(varnm, names(data))
        imsidata = data[,c(1,coln)]
        colnames(imsidata) = c("date", stnid)
        if(!all(is.na(imsidata[2]))){imsidata[, 2] = na.approx(imsidata[, 2], na.rm=F)}
        imsidata$date = as.Date(imsidata$date)

        vardata = merge(vardata, imsidata, by="date", all=T)
      }

    }

    vardata$date = as.Date(vardata$date)
    datacnt = nrow(vardata)
    year = as.numeric(format(vardata$date,"%Y"))
    julian = strptime(vardata$date, "%Y-%m-%d")$yday+1
    var = vardata[c(2:(stncnt+1))]
    var[is.na(var)] = -99.0

    ########### Write PCP file
    if(varnm == "prcp"){

      sink(paste(outdir, "/pcp1.pcp", sep=""))
      cat("Precipitation Input File pcp.pcp          created using rSWAT by Jaepil Cho", sep="\n")
      if(stncnt == 1){
        cat(paste("Lati", sprintf("%8.2f", Latis[1]), sep=""),sep="\n")
        cat(paste("Long", sprintf("%8.1f", Longs[1]), sep=""),sep="\n")
        cat(paste("Elev", sprintf("%8.0f", Elevs[1]), sep=""),sep="\n")
      } else {
        cat(paste("Lati", sprintf("%8.2f", Latis[1]), paste(sprintf("%5.2f", Latis[2:length(Latis)]), collapse=""), sep=""),sep="\n")
        cat(paste("Long", sprintf("%8.1f", Longs[1]), paste(sprintf("%5.1f", Longs[2:length(Longs)]), collapse=""), sep=""),sep="\n")
        cat(paste("Elev", sprintf("%8.0f", Elevs[1]), paste(sprintf("%5.0f", Elevs[2:length(Elevs)]), collapse=""), sep=""),sep="\n")
      }

      for(jj in 1:datacnt) {
        cat(paste(sprintf("%4d%03d", year[jj], julian[jj]), paste(sprintf("%05.1f", var[jj,]), collapse=""), sep=""), sep="\n")
      }
      sink()

    }

    ########### Write wnd file
    if(varnm == "wspd"){

      sink(paste(outdir, "/wnd.wnd", sep=""))
      cat("Wind Input File wnd.wnd          created using rSWAT by Jaepil Cho", sep="\n")
      for(jj in 1:datacnt) {
        cat(paste(sprintf("%4d%03d", year[jj], julian[jj]), paste(sprintf("%08.3f", var[jj,]), collapse=""), sep=""), sep="\n")
      }
      sink()

    }

    ########### Write hmd file
    if(varnm == "rhum"){
      sink(paste(outdir, "/hmd.hmd", sep=""))
      cat("Relative humidity input file hmd.hmd          created using rSWAT by Jaepil Cho", sep="\n")
      for(jj in 1:datacnt) {
        cat(paste(sprintf("%4d%03d", year[jj], julian[jj]), paste(sprintf("%08.3f", var[jj,]), collapse=""), sep=""), sep="\n")
      }
      sink()

    }

    ########### Write slr file
    if(varnm == "rsds"){
      sink(paste(outdir, "/slr.slr", sep=""))
      cat("Solar radition input file slr.slr          created using rSWAT by Jaepil Cho", sep="\n")
      for(jj in 1:datacnt) {
        cat(paste(sprintf("%4d%03d", year[jj], julian[jj]), paste(sprintf("%08.3f", var[jj,]), collapse=""), sep=""), sep="\n")
      }
      sink()

    }


  } # Variable Loop


  ###### ?µ? ?????? ####################
  remn = stncnt%%150
  if(remn == 0) nfile = stncnt%/%150
  if(remn > 0) nfile = stncnt%/%150 + 1

  for(j in 1:nfile){
    st = (j-1)*150+1
    if(stncnt <= (j*150)) {
      ed = stncnt
    } else {
      ed = j*150
    }
    tmpids = StnIDs[st:ed]
    tmpcnt = length(tmpids)
    tmplons = Longs[st:ed]
    tmplats = Latis[st:ed]
    tmpelevs = Elevs[st:ed]

    for(ii in 1:tmpcnt){

      stnid = tmpids[ii]

      if(!missing(fnamestr)){
        srchstr = paste("*", stnid, "*", fnamestr, ".csv", sep="")
      } else {
        srchstr = paste("*", stnid, "*.csv", sep="")
      }
      #srchstr = paste("*", stnid, "*", fnamestr, ".csv", sep="")
      InDFile = list.files(wthdir, pattern = glob2rx(srchstr), full.names=T)
      data = read.csv(InDFile, header=T, na.strings = c("-99.00", "-99.0", "-99"))
      if(ncol(data) == 12) {
        colnames(data) = c("year", "mon", "day", "prcp","tmax","tmin","wspd","rhum","rsds", "sshine", "cloud", "tavg")
        data$date = as.Date(sprintf("%d-%02d-%02d", data$year, data$mon, data$day))
        data = data[c("date", "prcp","tmax","tmin","wspd","rhum","rsds")]
      } else {
        data$date = as.Date(sprintf("%d-%02d-%02d", data$year, data$mon, data$day))
        data = data[c("date", "prcp","tmax","tmin","wspd","rhum","rsds")]
      }


      if(ii == 1){
        vardata = data[,c(1,match("tmax", names(data)), match("tmin", names(data)))]
        if(!all(is.na(vardata$tmax))){vardata[, c("tmax")] = na.approx(vardata[, c("tmax")], na.rm=F)}
        if(!all(is.na(vardata$tmin))){vardata[, c("tmin")] = na.approx(vardata[, c("tmin")], na.rm=F)}
        vardata$date = as.Date(vardata$date)
      } else {
        imsidata = data[,c(1,match("tmax", names(data)), match("tmin", names(data)))]
        if(!all(is.na(imsidata$tmax))){imsidata[, c("tmax")] = na.approx(imsidata[, c("tmax")], na.rm=F)}
        if(!all(is.na(imsidata$tmin))){imsidata[, c("tmin")] = na.approx(imsidata[, c("tmin")], na.rm=F)}
        imsidata$date = as.Date(imsidata$date)
        vardata = merge(vardata, imsidata, by="date", all=T)
      }
    } # Station Loop

    vardata$date = as.Date(vardata$date)
    datacnt = nrow(vardata)
    year = as.numeric(format(vardata$date,"%Y"))
    julian = strptime(vardata$date, "%Y-%m-%d")$yday+1
    var = vardata[c(2:(2*tmpcnt+1))]
    var[is.na(var)] = -99.0
    var[var=="NA"] = -99.0

    ########### Write PCP file
    sink(paste(outdir, sprintf("/tmp%d.tmp", j), sep=""))
    cat("Temperature Input File tmp.tmp          created using rSWAT by Jaepil Cho", sep="\n")
    if(stncnt == 1){
      cat(paste("Lati", sprintf("%13.7f", tmplats[1]), sep=""),sep="\n")
      cat(paste("Long", sprintf("%13.7f", tmplons[1]), sep=""),sep="\n")
      cat(paste("Elev", sprintf("%13.0f", tmpelevs[1]), sep=""),sep="\n")
    } else {
      cat(paste("Lati", sprintf("%13.7f", tmplats[1]), paste(sprintf("%10.7f", Latis[2:length(tmplats)]), collapse=""), sep=""),sep="\n")
      cat(paste("Long", sprintf("%13.7f", tmplons[1]), paste(sprintf("%10.7f", Longs[2:length(tmplons)]), collapse=""), sep=""),sep="\n")
      cat(paste("Elev", sprintf("%13.0f", tmpelevs[1]), paste(sprintf("%10.0f", Elevs[2:length(tmpelevs)]), collapse=""), sep=""),sep="\n")
    }
    for(jj in 1:datacnt) {
      cat(paste(sprintf("%4d%03d", year[jj], julian[jj]), paste(sprintf("%05.1f", var[jj,]), collapse=""), sep=""), sep="\n")
    }
    sink()

  }



}

###################################################################################################################
# write file.cio input file
###################################################################################################################
Write.Cio.Input <- function(wdir, NBYR, IYR, NYSKIP) {

  #NBYR = 9
  #IYR = 2003
  #NYSKIP = 0
  #wdir = EnvList$SwatRunDir

  EYR = IYR + NBYR - 1

  setwd(wdir)
  file.copy("file.cio", "file-temp.cio")


  con_tmp <- file(file.path(wdir, "file-temp.cio"), open="rt")
  con_tgt <- file(file.path(wdir, "file.cio"), open="wt")
  linn <-readLines(con_tmp)
  for (i in 1:length(linn)){
    if(length(grep("NBYR",linn[i])) > 0){
      writeLines(sprintf("             %3d    | NBYR : Number of years simulated", NBYR), con_tgt)
    } else if (length(grep("IYR",linn[i])) > 0){
      writeLines(sprintf("            %4d    | IYR : Beginning year of simulation", IYR), con_tgt)
    } else if(length(grep("IDAL",linn[i])) > 0){
      if((EYR %% 4) == 0) {
        writeLines(sprintf("             366    | IDAL : Ending julian day of simulation"), con_tgt)
      } else {
        writeLines(sprintf("             365    | IDAL : Ending julian day of simulation"), con_tgt)
      }
    } else if(length(grep("NYSKIP",linn[i])) > 0){
      writeLines(sprintf("             %3d    | NYSKIP: number of years to skip output printing/summarization", NYSKIP), con_tgt)
    } else {
      writeLines(linn[i], con_tgt)
    }
  }
  close(con_tmp)
  close(con_tgt)

  file.remove("file-temp.cio")
}


###################################################################################################################
# Iamhere write res input file for SWAT simulation
###################################################################################################################
Swat.Write.Rsv.Input <- function(wdir, resname, RES_ESA, RES_EVOL, RES_PSA, RES_PVOL) {

   # wdir <- "G:/SCService-RM/CliWARA_SWAT/Scenarios/Default"
   # resname <- "000070002.mgt"
   # RES_ESA = 2
   # RES_EVOL = 2
   # RES_PSA = 2
   # RES_PVOL = 2

   fnm <- substr(resname, 1, nchar(resname)-4)
   ext <- substr(resname, nchar(resname)-3, nchar(resname))
   TgtDFnm <- file.path(wdir, resname)
   TmpDFnm <- file.path(wdir, sprintf("%s-temp%s", fnm, ext))
   file.copy(TgtDFnm, TmpDFnm,overwrite = T)

  con_tmp <- file(file.path(wdir, "file-temp.cio"), open="rt")
  con_tgt <- file(file.path(wdir, "file.cio"), open="wt")
  linn <-readLines(con_tmp)
  for (i in 1:length(linn)){
    if(length(grep("RES_ESA",linn[i])) > 0){
      writeLines(sprintf("%16.1f    | RES_ESA : Reservoir surface area when the reservoir is filled to the  emergency spillway [ha]", RES_ESA), con_tgt)
    } else if (length(grep("RES_EVOL",linn[i])) > 0){
      writeLines(sprintf("%16.1f    | RES_EVOL : Volume of water needed to fill the reservoir to the emergency spillway (104 m3)", RES_EVOL), con_tgt)
    } else if (length(grep("RES_PSA",linn[i])) > 0){
      writeLines(sprintf("%16.1f    | RES_PSA : Reservoir surface area when the reservoir is filled to the principal spillway [ha]", RES_PSA), con_tgt)
    } else if(length(grep("RES_PVOL",linn[i])) > 0){
      writeLines(sprintf("%16.1f    | RES_PVOL : Volume of water needed to fill the reservoir to the principal spillway [104 m3]", RES_PVOL), con_tgt)
    } else {
      writeLines(linn[i], con_tgt)
    }
  }
  close(con_tmp)
  close(con_tgt)

  file.remove(con_tmp)


}


###################################################################################################################
# write mgt input file for SWAT simulation
###################################################################################################################
Swat.Write.Mgt.Input <- function(wdir, mgtname, IRRSC, IRRNO) {

  # wdir <- "G:/SCService-RM/CliWARA_SWAT/Scenarios/Default"
  # mgtname <- "000070002.mgt"
  # IRRSC = 2
  # IRRNO = 2

  fnm <- substr(mgtname, 1, nchar(mgtname)-4)
  ext <- substr(mgtname, nchar(mgtname)-3, nchar(mgtname))
  TgtDFnm <- file.path(wdir, mgtname)
  TmpDFnm <- file.path(wdir, sprintf("%s-temp%s", fnm, ext))
  file.copy(TgtDFnm, TmpDFnm,overwrite = T)

  con_tmp <- file(TmpDFnm, open="rt")
  con_tgt <- file(TgtDFnm, open="wt")
  linn <-readLines(con_tmp)
  for (i in 1:length(linn)){
    if(length(grep("IRRSC",linn[i])) > 0){
      writeLines(sprintf("%16.0f    | IRRSC: irrigation code", IRRSC), con_tgt)
    } else if (length(grep("IRRNO",linn[i])) > 0){
      writeLines(sprintf("%16.0f    | IRRNO: irrigation source location", IRRNO), con_tgt)
    } else {
      writeLines(linn[i], con_tgt)
    }
  }
  close(con_tmp)
  close(con_tgt)

  file.remove(TmpDFnm)


}



###################################################################################################################
# write measured daily reservoir outflow input file (resdayo.dat)
###################################################################################################################
Swat.Write.Resdayo.Input <- function(wdir, dayet, flowin, pvol, irrlossf, pdate, hdate) {

  #  pdate = as.Date("2007-04-20")
  #  hdate = as.Date("2011-09-30")
  #  irrlossf = 0.2
  #  pvol = 12000000
  foutf = 0.2

  datacnt = nrow(dayet)

  ########### if out of growing season, put zero values
  # put julian day into
  dayet["julian"] = strptime(dayet$date, "%Y-%m-%d")$yday+1
  # convert planting and harvest date into julian
  pdate = as.Date(pdate)
  pjul = strptime(pdate, "%Y-%m-%d")$yday+1
  hdate = as.Date(hdate)
  hjul = strptime(hdate, "%Y-%m-%d")$yday+1

  dayet$ETmm = ifelse((dayet$julian >= pjul & dayet$julian <= hjul), dayet$ETmm, 0)

  #write.csv(dayet, "res_ET_demands.csv", row.names=F)

  dayet$ETmm = dayet$ETmm * (1+irrlossf)

  ########### Initialize flowout as actual ET
  flowout = as.data.frame(dayet$ETmm)
  flowout = 0
  #curvol = data.frame(dayet$ETmm)


  #curvol = pvol
  prevol = pvol
  for(i in 1:datacnt){

    aet = dayet$ETmm[i]
    fin = flowin$FLOW_INm3[i]
    if(fin > 0) {
      fout = fin *foutf
    } else {
      fout = 0
    }

    if(aet == 0) {
      temp = prevol + fin
      if(temp <= pvol) {
        flowout[i] = fout
        prevol = temp - fout
      } else {
        flowout[i] = temp - pvol + fout
        prevol = pvol - fout
      }
    } else {
      temp = prevol + fin - aet
      if(temp <0){
        flowout[i] =  fout
      }
      if(temp <= pvol) {
        flowout[i] = aet + fout
        prevol = temp - fout
      }

      if (temp > pvol) {
        flowout[i] = temp - pvol + fout
        prevol = pvol - fout
      }
    }

  }

  ########### Write rsvdayo file

  setwd(wdir)
  sink("000020000.day")
  cat("Measured daily reservoir outflow input ", sep="\n")
  for(i in 1:datacnt) {
    cat(sprintf("%8.2f", flowout[i]/(60*60*24)), sep="\n")
  }
  sink()

  return(wdir)

}
