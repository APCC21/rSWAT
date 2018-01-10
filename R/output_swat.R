###################################################################################################################
# outtype: 1-flow, 2-Sediment, 3-TN, 4-TP
# funtype : 1-sum, 2-mean, 3-max, 4-min, 5-sd, 6-length
###################################################################################################################
Swat.Rch.Summary <- function(wdir, rchfile, outlet, sdate, outtype, funtype, sdate2=NULL, edate2=NULL, skiprow=NULL) {

  #   wdir = 'D:/2013-Gwangdong/SWAT_Gwangdong/Scenarios/Default/TxtInOut'
  #   sdate = as.Date("2009-01-01")
  #   sdate2 = as.Date("2010-01-01")
  #   edate2 = as.Date("2012-12-31")
  #   outlet = 23
  #   outtype = "flow"
  #   funtype = "sum"

  con_tmp <- file(file.path(wdir, rchfile), open="rt")
  linn <-readLines(con_tmp)
  for (i in 1:length(linn)){
    if(length(grep("GIS",linn[i])) > 0){
      colnms = strsplit(linn[i], "\\s+")
    }
  }
  close(con_tmp)

  sdate = as.Date(sdate)
  if(!missing(sdate2)){sdate2 = as.Date(sdate2)}
  #edate = as.Date(edate)
  if(!missing(edate2)){edate2 = as.Date(edate2)}

  setwd(wdir)

  if(!missing(skiprow)){
    file = tryCatch(read.table(rchfile, skip = skiprow, header=F), error=function(e) NULL)
  } else {
    #file = read.table("output.rch", skip = 9, header=F)
    file = tryCatch(read.table(rchfile, skip = 9, header=F), error=function(e) NULL)
  }

  if(is.null(file)) {

    outList = NULL

  } else {

    #file = file[which(file[,2]==as.numeric(outlet)),]
    file = file[which(file[,2]==outlet), 1:30]
    colnames(file) = colnms[[1]][1:30]
    nrows = nrow(file)
    data = seq(sdate, sdate+nrows-1, by =1)

    ######## Watershed area which might be used for unit conversion
    WSArea = file[1,c("AREAkm2")]


    ######## exctract only output type
    if(outtype == "water") {
      data = cbind.data.frame(data,file[, c("FLOW_OUTcms")])
      colnames(data) = c("date", "flow_cms")
    }

    if(outtype == "waterd") {
      #unit conversion: cms --> mm
      waterd = (file[, c("FLOW_OUTcms")] * 60 * 60 * 24) / (WSArea * 10^3)
      data = cbind.data.frame(data, waterd)
      colnames(data) = c("date", "flow_mm")
    }


    if (outtype == "sed") {
      data = cbind.data.frame(data, file[, c("SED_OUTtons")])
      colnames(data) = c("date", "Sed_ton")
    }

    if (outtype == "sedc") {
      sedc = (file[, c("SED_OUTtons")] * 10^6) / (file[,c("FLOW_OUTcms")] * 60 * 60 * 24)
      data = cbind.data.frame(data, sedc)
      colnames(data) = c("date", "Sed_mgl")
    }

    if (outtype == "tn") {
      data = cbind.data.frame(data,file[,c("ORGN_OUTkg", "NO3_OUTkg", "NH4_OUTkg", "NO2_OUTkg")])
      colnames(data) = c("date", "OrgN_kg", "NO3_kg", "N4_kg", "NO2_kg")
      data$TN_kg = data[,2]+data[,3]+data[,4]+data[,5]
    }

    if (outtype == "tnc") {
      tnc = (file[,c("ORGN_OUTkg", "NO3_OUTkg", "NH4_OUTkg", "NO2_OUTkg")] * 10^3) / (file[,c("FLOW_OUTcms")] * 60 * 60 * 24)
      data = cbind.data.frame(data, tnc)
      colnames(data) = c("date", "OrgN_mgl", "NO3_mgl", "NH4_mgl", "NO2_mgl")
      data$TN_mgl = data[,2]+data[,3]+data[,4]+data[,5]
    }

    if (outtype == "tp") {
      data = cbind.data.frame(data,file[,c("ORGP_OUTkg", "MINP_OUTkg")])
      colnames(data) = c("date", "OrgP_kg", "MinP_kg")
      data$TP_kg = data[,2]+data[,3]
    }

    if (outtype == "tpc") {
      tpc = (file[,c("ORGP_OUTkg", "MINP_OUTkg")] * 10^3) / (file[,c("FLOW_OUTcms")] * 60 * 60 * 24)
      data = cbind.data.frame(data, tpc)
      colnames(data) = c("date", "OrgP_mgl", "MinP_mgl")
      data$TP_mgl = data[,2]+data[,3]
    }

    ######## fill date columns
    data = Obs.Fill.Date(data)

    ######## exclue data based on give period options
    if(!missing(sdate2)) {
      sdate2 = as.Date(sdate2)
      if (as.numeric(sdate2) - as.numeric(sdate) < 0) {sdate2=sdate}
      data = data[which(data[,c("date")]>=sdate2),]
    }
    if(!missing(edate2)) {
      edate2 = as.Date(edate2)
      data = data[which(data[,c("date")]<=edate2),]
    }


    ########## Create aggregated data
    if(outtype == "water") {
      ydata = aggregate(flow_cms ~ year, data = data, FUN = funtype)
      mdata = aggregate(flow_cms ~ month, data = data, FUN = funtype)
      ymdata = aggregate(flow_cms ~ yearmon, data = data, FUN = funtype)
    }

    if(outtype == "waterd") {
      ydata = aggregate(flow_mm ~ year, data = data, FUN = funtype)
      mdata = aggregate(flow_mm ~ month, data = data, FUN = funtype)
      ymdata = aggregate(flow_mm ~ yearmon, data = data, FUN = funtype)
    }

    if (outtype == "sed") {
      ydata = aggregate(Sed_ton ~ year, data = data, FUN = funtype)
      mdata = aggregate(Sed_ton ~ month, data = data, FUN = funtype)
      ymdata = aggregate(Sed_ton ~ yearmon, data = data, FUN = funtype)
    }

    if (outtype == "sedc") {
      ydata = aggregate(Sed_mgl ~ year, data = data, FUN = funtype)
      mdata = aggregate(Sed_mgl ~ month, data = data, FUN = funtype)
      ymdata = aggregate(Sed_mgl ~ yearmon, data = data, FUN = funtype)
    }

    if (outtype == "tn") {
      ydata = aggregate(cbind(OrgN_kg, NO3_kg, NH4_kg, NO2_kg, TN_kg)~ year, data = data, FUN = funtype)
      mdata = aggregate(cbind(OrgN_kg, NO3_kg, NH4_kg, NO2_kg, TN_kg) ~ month, data = data, FUN = funtype)
      ymdata = aggregate(cbind(OrgN_kg, NO3_kg, NH4_kg, NO2_kg, TN_kg) ~ yearmon, data = data, FUN = funtype)
    }

    if (outtype == "tnc") {
      ydata = aggregate(cbind(OrgN_mgl, NO3_mgl, NH4_mgl, NO2_mgl, TN_mgl)~ year, data = data, FUN = funtype)
      mdata = aggregate(cbind(OrgN_mgl, NO3_mgl, NH4_mgl, NO2_mgl, TN_mgl) ~ month, data = data, FUN = funtype)
      ymdata = aggregate(cbind(OrgN_mgl, NO3_mgl, NH4_mgl, NO2_mgl, TN_mgl) ~ yearmon, data = data, FUN = funtype)
    }

    if (outtype == "tp") {
      ydata = aggregate(cbind(OrgP_kg, MinP_kg, TP_kg)~ year, data = data, FUN = funtype)
      mdata = aggregate(cbind(OrgP_kg, MinP_kg, TP_kg) ~ month, data = data, FUN = funtype)
      ymdata = aggregate(cbind(OrgP_kg, MinP_kg, TP_kg) ~ yearmon, data = data, FUN = funtype)
    }

    if (outtype == "tpc") {
      ydata = aggregate(cbind(OrgP_mgl, MinP_mgl, TP_mgl)~ year, data = data, FUN = funtype)
      mdata = aggregate(cbind(OrgP_mgl, MinP_mgl, TP_mgl) ~ month, data = data, FUN = funtype)
      ymdata = aggregate(cbind(OrgP_mgl, MinP_mgl, TP_mgl) ~ yearmon, data = data, FUN = funtype)
    }


    ########### add count
    ydata["count"] = NA
    yrow = nrow(ydata)
    for (i in 1:yrow) { ydata[i,ncol(ydata)] = length(which(data[,1]==ydata[i,1])) }

    mdata["count"] = NA
    mrow = nrow(mdata)
    for (i in 1:mrow) { mdata[i,ncol(mdata)] = length(which(data[,2]==mdata[i,1])) }

    ymdata["count"] = NA
    ymrow = nrow(ymdata)
    for (i in 1:ymrow) {
      cyear = as.numeric(substr(ymdata[i,1], 1, 4))
      cmon = as.numeric(substr(ymdata[i,1], 6, 8))
      ymdata[i,ncol(mdata)] = length(which(data[,1]==cyear & data[,2]==cmon))
    }

    outList = list("file"=file, "data"= data, "ydata"= ydata, "mdata"= mdata, "ymdata" = ymdata, "area"=WSArea)

  }


  return(outList)

}

###################################################################################################################
# outtype: 1-flow, 2-Sediment, 3-TN, 4-TP
# funtype : 1-sum, 2-mean, 3-max, 4-min, 5-sd, 6-length
###################################################################################################################
Swat.Sub.Summary <- function(wdir, outlet, sdate, outtype, funtype, sdate2=NULL, edate2=NULL, skiprow=NULL) {


  #     wdir = 'D:/2013-Gwangdong/SWAT_Gwangdong/Scenarios/Default/TxtInOut'
  #     sdate = as.Date("2009-01-01")
  #     sdate2 = as.Date("2010-01-01")
  #     edate2 = as.Date("2012-12-31")
  #     outlet = 23
  #     outtype = "tn"
  #     funtype = "sum"


  #   wdir = 'D:/SWAT-SMG/SWAT-Field/Scenarios/Default/TxtInOut'
  #   sdate = "2011-01-01"
  #   sdate2 = "2011-01-01"
  #   edate2 = "2011-12-31"
  #   outlet = 1
  #   outtype = "sed"
  #   funtype = "mean"

  cwidth = c(6,4,9,5,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10)
  colnms = c("type", "SUB", "GIS", "MON", "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm",
             "PERCmm", "SURQmm", "GW_Qmm", "WYLDmm", "SYLDt/ha", "ORGNkg/ha", "ORGPkg/ha", "NSURQkg/ha", "SOLPkg/ha", "SEDPkg/ha",
             "LAT Q(mm)", "LATNO3kg/h", "GWNO3kg/ha", "CHOLAkg/ha", "CBODUkg/ha", "DOXQkg/ha")

  sdate = as.Date(sdate)


  setwd(wdir)

  if(!missing(skiprow)){
    file = read.fwf("output.sub", widths=cwidth, skip = skiprow, header=T)
  } else {
    file = read.fwf("output.sub", widths=cwidth, skip = 9, header=F)
  }

  file = file[which(file[,2]==outlet),]
  colnames(file) = colnms
  nrow = nrow(file)
  data = seq(sdate, sdate+nrow-1, by =1)

  ######## Watershed area which might be used for unit conversion
  WSArea = file[1,5]


  ######## exctract only output type
  if(outtype == "water") {
    data = cbind.data.frame(data,file[,c(6:12, 21, 13:14)])
    colnames(data) = c("date", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm", "PERCmm", "SURQmm", "LAT_Qmm", "GW_Qmm", "WYLDmm")
  }

  if (outtype == "sed") {
    data = cbind.data.frame(data,file[,15])
    colnames(data) = c("date", "SYLDtha")
  }

  if (outtype == "tn") {
    data = cbind.data.frame(data,file[,c(16, 18, 22, 23)])
    colnames(data) = c("date", "ORGNkgha", "NSURQkgha", "LATNO3kgha", "GWNO3kgha")
    data$TNkgha = data[,2]+data[,3]+data[,4]+data[,5]
  }

  if (outtype == "tp") {
    data = cbind.data.frame(data,file[,c(17, 19, 20)])
    colnames(data) = c("date", "ORGPkgha", "SOLPkgha", "SEDPkgha")
    data$TPkgha = data[,2]+data[,3]+data[,4]
  }

  ######## fill date columns
  data = Obs.Fill.Date(data)

  ######## exclue data based on give period options
  if(!missing(sdate2)) {
    sdate2 = as.Date(sdate2)
    if (as.numeric(sdate2) - as.numeric(sdate) < 0) {sdate2=sdate}
    data = data[which(data[,4]>=sdate2),]
  }
  if(!missing(edate2)) {
    edate2 = as.Date(edate2)
    data = data[which(data[,4]<=edate2),]
  }


  ########## Create aggregated data
  if(outtype == "water") {
    ydata = aggregate(cbind(PRECIPmm, SNOMELTmm, PETmm, ETmm, SWmm, PERCmm, SURQmm, LAT_Qmm, GW_Qmm, WYLDmm) ~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(PRECIPmm, SNOMELTmm, PETmm, ETmm, SWmm, PERCmm, SURQmm, LAT_Qmm, GW_Qmm, WYLDmm) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(PRECIPmm, SNOMELTmm, PETmm, ETmm, SWmm, PERCmm, SURQmm, LAT_Qmm, GW_Qmm, WYLDmm) ~ yearmon, data = data, FUN = funtype)
  }

  if (outtype == "sed") {
    ydata = aggregate(SYLDtha ~ year, data = data, FUN = funtype)
    mdata <- aggregate(SYLDtha ~ month, data = data, FUN = funtype)
    ymdata = aggregate(SYLDtha ~ yearmon, data = data, FUN = funtype)
  }

  if (outtype == "tn") {
    ydata = aggregate(cbind(ORGNkgha, NSURQkgha, LATNO3kgha, GWNO3kgha, TNkgha)~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(ORGNkgha, NSURQkgha, LATNO3kgha, GWNO3kgha, TNkgha) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(ORGNkgha, NSURQkgha, LATNO3kgha, GWNO3kgha, TNkgha) ~ yearmon, data = data, FUN = funtype)
  }

  if (outtype == "tp") {
    ydata = aggregate(cbind(ORGPkgha, SOLPkgha, SEDPkgha, TPkgha)~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(ORGPkgha, SOLPkgha, SEDPkgha, TPkgha) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(ORGPkgha, SOLPkgha, SEDPkgha, TPkgha) ~ yearmon, data = data, FUN = funtype)
  }

  ########### add count
  ydata["count"] = NA
  yrow = nrow(ydata)
  for (i in 1:yrow) { ydata[i,ncol(ydata)] = length(which(data[,1]==ydata[i,1])) }

  mdata["count"] = NA
  mrow = nrow(mdata)
  for (i in 1:mrow) { mdata[i,ncol(mdata)] = length(which(data[,2]==mdata[i,1])) }

  ymdata["count"] = NA
  ymrow = nrow(ymdata)
  for (i in 1:ymrow) {
    cyear = as.numeric(substr(ymdata[i,1], 1, 4))
    cmon = as.numeric(substr(ymdata[i,1], 6, 8))
    ymdata[i,ncol(mdata)] = length(which(data[,1]==cyear & data[,2]==cmon))
  }

  outList = list("file"=file, "data"= data, "ydata"= ydata, "mdata"= mdata, "ymdata" = ymdata, "area"=WSArea)

  return(outList)

}

###################################################################################################################
# outtype: year, month, yearmon, sub
###################################################################################################################
Swat.Sub.Summary2 <- function(wdir, sdate, edate, outtype, skiprow=NULL) {

  #   wdir = "D:/SWAT-SMG/SWAT-WQ-Mankyong/Scenarios/Default/CalInOut"
  sdate = as.Date(sdate)
  edate = as.Date(edate)

  library(reshape)

  cwidth = c(6,4,9,5,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10, 10,10,10,11,10,10,10)
  colnms = c("type", "SUB", "GIS", "MON", "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm",
             "PERCmm", "SURQmm", "GWQmm", "WYLDmm", "SYLDtha", "ORGNkgha", "ORGPkgha", "NSURQkgha", "SOLPkgha", "SEDPkgha",
             "LATQmm", "LATNO3kgh", "GWNO3kgha", "CHOLAmicL", "CBODUmgL", "DOXQmgL", "TNO3kgha")
  #   colnms = c("type", "SUB", "GIS", "MON", "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm",
  #              "PERCmm", "SURQmm", "GW_Qmm", "WYLDmm", "SYLDt/ha", "ORGNkg/ha", "ORGPkg/ha", "NSURQkg/ha", "SOLPkg/ha", "SEDPkg/ha",
  #              "LAT Q(mm)", "LATNO3kg/h", "GWNO3kg/ha", "CHOLAmic/L", "CBODU mg/L", "DOXQ mg/L", "TNO3kg/ha")

  setwd(wdir)

  if(!missing(skiprow)){
    data = read.fwf("output.sub", widths=cwidth, skip = skiprow, header=T)
  } else {
    data = read.fwf("output.sub", widths=cwidth, skip = 9, header=F)
  }

  colnames(data) = colnms

  data = subset(data, select = -c(type,GIS,MON,CHOLAmicL,CBODUmgL,DOXQmgL,TNO3kgha,SNOMELTmm,PETmm,SWmm))

  nrows = nrow(data)
  ndays = as.numeric(edate - sdate + 1)
  nyear = as.numeric (format(edate,"%Y")) - as.numeric (format(sdate,"%Y")) + 1
  nsubs = nrows / ndays
  date = rep(seq(sdate, sdate+ndays-1, by=1), each=nsubs)
  data = cbind(date, data)
  data = Obs.Fill.Date(data)

  data = melt(data, id=c("year", "month", "yearmon", "date", "SUB"))

  # subbasins area (km2)
  #   data = cast(data, SUB~variable, mean)
  #   data = data$AREAkm2

  if(outtype == "sub"){
    data = cast(data, SUB~variable, sum)
    data$AREAkm2 = data$AREAkm2 / ndays
    data[,3:ncol(data)] = data[,3:ncol(data)] / nyear

    data$tp = data[, "ORGPkgha"] + data[, "SOLPkgha"] + data[, "SEDPkgha"]
    data$tn_suf = data[, "ORGNkgha"] + data[, "NSURQkgha"]
    data$tn = data[, "ORGNkgha"] + data[, "NSURQkgha"] + data[, "LATNO3kgh"] + data[, "GWNO3kgha"]
  }
  #   if(outtype == "year"){
  #     subdata = cast(data, year~variable, sum)
  #   }
  #   if(outtype == "month"){
  #     subdata = cast(data, month~variable, sum)
  #   }
  #   if(outtype == "yearmon"){
  #     subdata = cast(data, yearmon~variable, sum)
  #   }

  #  outList = list("data"= data, "subdata"= subdata, "subarea"=subarea)

  return(data)

}

###################################################################################################################
# you have to configure "HRU output variables" part of file.cio input file as below
#     HRU output variables:
#     1   2   4   5   6   9   0   0   0   0   0   0   0   0   0   0   0   0   0   0
# outtype: 1-flow, 2-Sediment, 3-TN, 4-TP
# funtype : 1-sum, 2-mean, 3-max, 4-min, 5-sd, 6-length
###################################################################################################################
Swat.Hru.Summary <- function(wdir, lutype, sdate, outtype, funtype, sdate2=NULL, edate2=NULL, skiprow=NULL) {


  #   wdir = 'D:/2013-AgReservoirs/SWAT_Historical/201909/Scenarios/Default/TxtInOut'
  #   sdate = as.Date("2009-01-01")
  #   sdate2 = as.Date("2010-01-01")
  #   edate2 = as.Date("2012-12-31")
  #   lutype = "PADY"
  #   outtype = "water"
  #   funtype = "sum"

  cwidth = c(4,5,13,5,5,5,10,10,10,10, 10,10,10)
  colnms = c("LULC", "HRU", "GIS", "SUB", "MGT", "MON", "AREAkm2", "PRECIPmm", "SNOFALLmm", "IRRmm",
             "PETmm", "ETmm", "PERCmm")

  sdate = as.Date(sdate)


  setwd(wdir)

  if(!missing(skiprow)){
    file = read.fwf("output.hru", widths=cwidth, skip = skiprow, header=F)
  } else {
    file = read.fwf("output.hru", widths=cwidth, skip = 9, header=F)
  }

  colnames(file) = colnms

  # Only one land use type is considered in an HRU output file
  #file = file[which(file[,1]==lutype),]

  ######## Calucalte daily average through weighting factor using HRU area #################
  imsi = aggregate(AREAkm2 ~ GIS, data = file, FUN = mean)
  hrucnt = nrow(imsi)
  hruarea = colSums(imsi)[2]

  nday = nrow(file)/hrucnt
  date = as.character(seq(sdate, sdate+nday-1, by =1))

  file$date = rep(date, each = hrucnt)

  file[,8:13] = file[,8:13] * file[,7]

  data = aggregate(cbind(PRECIPmm, SNOFALLmm, IRRmm, PETmm,  ETmm, PERCmm) ~ date, data = file, FUN = sum)
  data[,2:7] = data[,2:7]/hruarea


  ######## fill date columns
  data = Obs.Fill.Date(data)

  ######## exclue data based on give period options
  if(!missing(sdate2)) {
    sdate2 = as.Date(sdate2)
    if (as.numeric(sdate2) - as.numeric(sdate) < 0) {sdate2=sdate}
    data = data[which(data[,4]>=sdate2),]
  }
  if(!missing(edate2)) {
    edate2 = as.Date(edate2)
    data = data[which(data[,4]<=edate2),]
  }


  ########## Create aggregated data
  if(outtype == "water") {
    ydata = aggregate(cbind(PRECIPmm, SNOFALLmm, IRRmm, PETmm,  ETmm, PERCmm) ~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(PRECIPmm, SNOFALLmm, IRRmm, PETmm,  ETmm, PERCmm) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(PRECIPmm, SNOFALLmm, IRRmm, PETmm,  ETmm, PERCmm) ~ yearmon, data = data, FUN = funtype)
  }


  ########### add count
  ydata["count"] = NA
  yrow = nrow(ydata)
  for (i in 1:yrow) { ydata[i,ncol(ydata)] = length(which(data[,1]==ydata[i,1])) }

  mdata["count"] = NA
  mrow = nrow(mdata)
  for (i in 1:mrow) { mdata[i,ncol(mdata)] = length(which(data[,2]==mdata[i,1])) }

  ymdata["count"] = NA
  ymrow = nrow(ymdata)
  for (i in 1:ymrow) {
    cyear = as.numeric(substr(ymdata[i,1], 1, 4))
    cmon = as.numeric(substr(ymdata[i,1], 6, 8))
    ymdata[i,ncol(mdata)] = length(which(data[,1]==cyear & data[,2]==cmon))
  }

  outList = list("file"=file, "data"= data, "ydata"= ydata, "mdata"= mdata, "ymdata" = ymdata, "area"=hruarea)

  return(outList)

}

###################################################################################################################
# outtype: 1-water, 2-Sed, 3-TN, 4-TP
# funtype : 1-sum, 2-mean, 3-max, 4-min, 5-sd, 6-length
###################################################################################################################
Swat.Rsv.Summary <- function(wdir, rsvno, sdate, outtype, funtype, sdate2=NULL, edate2=NULL, skiprow=NULL) {


  #   wdir = 'D:/2013-AgReservoirs/SWAT_Historical/201909/Scenarios/Default/TxtInOut'
  #   sdate = as.Date("1979-01-01")
  #   sdate2 = as.Date("2010-01-01")
  #   edate2 = as.Date("2012-12-31")
  #   rsvno = 1
  #   outtype = "water"
  #   funtype = "sum"

  cwidth = c(8,6,5,12,12,12,12,12,12,12, 12,12,12,12,12,12,12,12,12,12, 12,12,12,12,12,12,12,12,12,12, 12,12,12,12,12,12,12,12,12,12, 12,12,12,12,12)
  colnms = c("type", "RES", "MON", "VOLUMEm3", "FLOW_INcms", "FLOW_OUTcms", "PRECIPm3", "EVAPm3", "SEEPAGEm3", "SED_INtons",
             "SED_OUTtons", "SED_CONCppm", "ORGN_INkg", "ORGN_OUTkg", "RES_ORGNppm", "ORGP_INkg", "ORGP_OUTkg", "RES_ORGPppm", "NO3_INkg", "NO3_OUTkg",
             "RES_NO3ppm", "NO2_INkg", "NO2_OUTkg", "RES_NO2ppm", "NH3_INkg", "NH3_OUTkg", "RES_NH3ppm", "MINP_INkg", "MINP_OUTkg", "RES_MINPppm",
             "CHLA_INkg", "CHLA_OUTkg", "SECCHIDEPTHm", "PEST_INmg", "REACTPSTmg", "VOLPSTmg", "SETTLPSTmg", "RESUSP_PSTmg", "DIFFUSEPSTmg", "REACBEDPSTmg",
             "BURYPSTmg", "PEST_OUTmg", "PSTCNCWmg/m3", "PSTCNCBmg/m3", "YR")

  sdate = as.Date(sdate)


  setwd(wdir)

  if(!missing(skiprow)){
    file = read.fwf("output.rsv", widths=cwidth, skip = skiprow, header=F)
  } else {
    file = read.fwf("output.rsv", widths=cwidth, skip = 9, header=F)
  }

  colnames(file) = colnms
  file = file[which(file[,2]==rsvno),]
  nrow = nrow(file)
  data = seq(sdate, sdate+nrow-1, by =1)

  ######## exctract only output type
  if(outtype == "water") {
    data = cbind.data.frame(data,file[,c(4:9)])
    # unit conversion cms --> m3
    data[,3:4] = data[,3:4] *60*60*24
    colnames(data) = c("date", "VOLUMEm3", "FLOW_INm3", "FLOW_OUTm3", "PRECIPm3", "EVAPm3", "SEEPAGEm3")
  }

  if (outtype == "sed") {
    data = cbind.data.frame(data,file[,c(10:11)])
    colnames(data) = c("date", "SED_INtons", "SED_OUTtons")
  }

  if (outtype == "tn") {
    data = cbind.data.frame(data,file[,c(13:14, 19:20, 22:23, 25:26)])
    colnames(data) = c("date", "ORGN_INkg", "ORGN_OUTkg", "NO3_INkg", "NO3_OUTkg", "NO2_INkg", "NO2_OUTkg", "NH3_INkg", "NH3_OUTkg")
    data$TN_INkg = data[,2]+data[,4]+data[,6]+data[,8]
    data$TN_OUTkg = data[,3]+data[,5]+data[,7]+data[,9]
  }

  if (outtype == "tp") {
    data = cbind.data.frame(data,file[,c(16:17, 28:29)])
    colnames(data) = c("date", "ORGP_INkg", "ORGP_OUTkg", "MINP_INkg", "MINP_OUTkg")
    data$TP_INkg = data[,2]+data[,4]
    data$TP_OUTkg = data[,3]+data[,5]
  }

  ######## fill date columns
  data = Obs.Fill.Date(data)
  data = data[order(data$date),]

  ######## exclue data based on give period options
  if(!missing(sdate2)) {
    sdate2 = as.Date(sdate2)
    if (as.numeric(sdate2) - as.numeric(sdate) < 0) {sdate2=sdate}
    data = data[which(data[,4]>=sdate2),]
  }
  if(!missing(edate2)) {
    edate2 = as.Date(edate2)
    data = data[which(data[,4]<=edate2),]
  }


  ########## Create aggregated data
  if(outtype == "water") {
    ydata = aggregate(cbind(VOLUMEm3, FLOW_INm3, FLOW_OUTm3, PRECIPm3, EVAPm3, SEEPAGEm3) ~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(VOLUMEm3, FLOW_INm3, FLOW_OUTm3, PRECIPm3, EVAPm3, SEEPAGEm3) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(VOLUMEm3, FLOW_INm3, FLOW_OUTm3, PRECIPm3, EVAPm3, SEEPAGEm3) ~ yearmon, data = data, FUN = funtype)
  }

  if (outtype == "sed") {
    ydata = aggregate(cbind(SED_INtons, SED_OUTtons) ~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(SED_INtons, SED_OUTtons) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(SED_INtons, SED_OUTtons) ~ yearmon, data = data, FUN = funtype)
  }

  if (outtype == "tn") {
    ydata = aggregate(cbind(ORGN_INkg, ORGN_OUTkg, NO3_INkg, NO3_OUTkg, NO2_INkg, NO2_OUTkg, NH3_INkg, NH3_OUTkg, TN_INkg, TN_OUTkg)~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(ORGN_INkg, ORGN_OUTkg, NO3_INkg, NO3_OUTkg, NO2_INkg, NO2_OUTkg, NH3_INkg, NH3_OUTkg, TN_INkg, TN_OUTkg) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(ORGN_INkg, ORGN_OUTkg, NO3_INkg, NO3_OUTkg, NO2_INkg, NO2_OUTkg, NH3_INkg, NH3_OUTkg, TN_INkg, TN_OUTkg) ~ yearmon, data = data, FUN = funtype)
  }

  if (outtype == "tp") {
    ydata = aggregate(cbind(ORGP_INkg, ORGP_OUTkg, MINP_INkg, MINP_OUTkg, TP_INkg, TP_OUTkg)~ year, data = data, FUN = funtype)
    mdata <- aggregate(cbind(ORGP_INkg, ORGP_OUTkg, MINP_INkg, MINP_OUTkg, TP_INkg, TP_OUTkg) ~ month, data = data, FUN = funtype)
    ymdata = aggregate(cbind(ORGP_INkg, ORGP_OUTkg, MINP_INkg, MINP_OUTkg, TP_INkg, TP_OUTkg) ~ yearmon, data = data, FUN = funtype)
  }

  ########### add count
  ydata["count"] = NA
  yrow = nrow(ydata)
  for (i in 1:yrow) { ydata[i,ncol(ydata)] = length(which(data[,1]==ydata[i,1])) }

  mdata["count"] = NA
  mrow = nrow(mdata)
  for (i in 1:mrow) { mdata[i,ncol(mdata)] = length(which(data[,2]==mdata[i,1])) }

  ymdata["count"] = NA
  ymrow = nrow(ymdata)
  for (i in 1:ymrow) {
    cyear = as.numeric(substr(ymdata[i,1], 1, 4))
    cmon = as.numeric(substr(ymdata[i,1], 6, 8))
    ymdata[i,ncol(mdata)] = length(which(data[,1]==cyear & data[,2]==cmon))
  }

  outList = list("file"=file, "data"= data, "ydata"= ydata, "mdata"= mdata, "ymdata" = ymdata)


  return(outList)

}

###################################################################################################################
# outtype: 1-flow, 2-Sediment, 3-TN, 4-TP
# funtype : 1-sum, 2-mean, 3-max, 4-min, 5-sd, 6-length
###################################################################################################################
Swat.Calc.Error <- function(data) {

  data = na.omit(data)

  obs = data[,2]
  sim = data[,3]

  obsavg = mean(obs)
  simavg = mean(sim)

  obssum = sum(obs)
  simsum = sum(sim)

  obs2 = obs^2
  sim2 = sim^2

  absdif = abs(sim-obs)

  dif2 = (obs-sim)^2
  obsdif2 = (obs-obsavg)^2

  error = (simsum - obssum)/obssum *100
  mse = mean(absdif)
  rmse = sqrt(mean(dif2))
  rsr = rmse/obsavg
  nof = sqrt(sum(dif2))/sqrt(sum(obsdif2))
  nse = 1- sum(dif2)/sum(obsdif2)

  outList = list("error"=error, "mse"= mse, "rmse"= rmse, "rsr"= rsr, "nof" = nof, "nse"=nse)


  return(outList)

}
