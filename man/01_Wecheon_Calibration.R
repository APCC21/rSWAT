
#########################################################################
################# 00. Prepare Obs data      ############################
#########################################################################


################# WAMIS DATA ############################
wlstnid = "2008690" #Younggok
wlstnid = "2008650" #Museong

#Check Wamis flow data and decide period #####
wamis = myGetWamisFlow(wlstnid)
#wamis = myGetWamisFlow(wlstnid, "2010-01-01", "2011-12-31")
plot = myGraphWamisFlow(wamis)

################# TMDL DATA ############################
wqstnid = "��õA"
wqstnid = "��õB"
#Check Wamis flow data and decide period #####
tmdl = myGetTmdlWQuality(wqstnid)
#tmdl = myGetTmdlWQuality(wqstnid, "2010-01-01", "2011-12-31")

#########################################################################
################# 01. Modeling configuration ############################
#########################################################################

library(sfsmisc)

wamisdir = "F:/StationDB/streamflow/wamis"
tmdldir = "F:/StationDB/wquality/tmdl"
swatdbdir = "F:/SWATDB"
prjdir = "F:/SWAT-NakDong/2008"

exedir = paste(swatdbdir, "/0_code-exe", sep="")
simdir = paste(prjdir, "/Scenarios/Default/TxtInOut", sep="")
caldir = paste(prjdir, "/Scenarios/Default/CalInOut", sep="")
caldir = SetWorkingDir(caldir)
valdir = paste(prjdir, "/Scenarios/Default/ValInOut", sep="")
valdir = SetWorkingDir(valdir)
inputdir = paste(prjdir, "/Scenarios/Default/UserInput", sep="")
inputdir = SetWorkingDir(inputdir)

#################copy SWAT exe file ##########################################
Srcfile = paste(exedir, "/swat591-64.exe", sep="")
Dstnfile = paste(simdir, "/swat591-64.exe", sep="")
file.copy(Srcfile, Dstnfile, overwrite=T)


for(i in 1:2){

  # calibration
  if(i == 1) {
    outdir = caldir
    sdate = "2005-01-01"
    sdate2 = "2007-01-01"
    edate = "2008-12-31"
    edate2 = edate
    maintitle = "Wecheon-Calibration"
    simtype = "Cal"
  # validation
  } else {
    outdir = valdir
    sdate = "2008-01-01"
    sdate2 = "2010-01-01"
    edate = "2011-12-31"
    edate2 = edate
    maintitle = "Wecheon-Validation"
    simtype = "Val"
  }


  ################# Update cio file  ###########################
  IYR = as.numeric(substr(sdate,1,4))
  NBYR = as.numeric(substr(edate,1,4)) - as.numeric(substr(sdate,1,4)) + 1
  NYSKIP = as.numeric(substr(sdate2,1,4)) - as.numeric(substr(sdate,1,4))
  # Update .cio input file(wdir, NBYR, IYR, NYSKIP,# NRTOT, NTTOT, NSTOT, NHTOT, NWTOT, HRU_min, HRU_max)
  cio = myWriteSwatCioInput_Local(simdir, NBYR, IYR, NYSKIP, 4, 4, 4, 4, 4, 1, 1)

  #################copy bsn file################################################
  calfile = paste(caldir, "/calibration.bsn", sep="")
  outfile = paste(simdir, "/basins.bsn", sep="")
  file.copy(calfile, outfile, overwrite=T)

  ################# GW change #############################################
  srchstr = "HLIFE_NGW"
  valposition = 16
  valformat = "%16.4f"
  fixedval = 0.0

  indir = paste(caldir, "/gw", sep="")
  flist = list.files(indir, pattern = glob2rx("*.gw"), full.names=F)
  fcnt = length(flist)
  for(i in 1:fcnt){
    fname = flist[i]
    InDFile = paste(indir, "/", fname, sep="")
    OutDFile = paste(simdir, "/", fname, sep="")
    myFixedChangeParameter(InDFile, OutDFile, srchstr,valposition, valformat, fixedval)
  }

  ################# CN2.mgt change #############################################
  srchstr = "CN2: Initial"
  valposition = 16
  valformat = "%16.2f"
  pincrease = -5
  valmax = 95
  valmin = 0

  indir = paste(caldir, "/mgt", sep="")
  flist = list.files(indir, pattern = glob2rx("*.mgt"), full.names=F)
  fcnt = length(flist)
  for(i in 1:fcnt){
    fname = flist[i]
    InDFile = paste(indir, "/", fname, sep="")
    OutDFile = paste(simdir, "/", fname, sep="")
    myPercentChangeParameter(InDFile, OutDFile, srchstr,valposition, valformat, valmin, valmax, pincrease)
  }

  ################# HRU change #############################################
  srchstr = "SLSUBBSN"
  valposition = 16
  valformat = "%16.3f"
  pincrease = 0
  valmax = 200
  valmin = 0

  indir = paste(caldir, "/hru", sep="")
  flist = list.files(indir, pattern = glob2rx("*.hru"), full.names=F)
  fcnt = length(flist)
  for(i in 1:fcnt){
    fname = flist[i]
    InDFile = paste(indir, "/", fname, sep="")
    OutDFile = paste(simdir, "/", fname, sep="")
    myPercentChangeParameter(InDFile, OutDFile, srchstr,valposition, valformat, valmin, valmax, pincrease)
  }

  ################# SWQ change #############################################
  srchstr = "BC3"
  valposition = 16
  valformat = "%16.3f"
  pincrease = 80
  valmax = 200
  valmin = 0

  indir = paste(caldir, "/swq", sep="")
  flist = list.files(indir, pattern = glob2rx("*.swq"), full.names=F)
  fcnt = length(flist)
  for(i in 1:fcnt){
    fname = flist[i]
    InDFile = paste(indir, "/", fname, sep="")
    OutDFile = paste(simdir, "/", fname, sep="")
    myPercentChangeParameter(InDFile, OutDFile, srchstr,valposition, valformat, valmin, valmax, pincrease)
  }


  ################# RTE change #############################################
  srchstr = "CH_K2"
  valposition = 14
  valformat = "%14.3f"
  fixedval = 0.0

  indir = paste(caldir, "/rte", sep="")
  flist = list.files(indir, pattern = glob2rx("*.rte"), full.names=F)
  fcnt = length(flist)
  for(i in 1:fcnt){
    fname = flist[i]
    InDFile = paste(indir, "/", fname, sep="")
    OutDFile = paste(simdir, "/", fname, sep="")
    myFixedChangeParameter(InDFile, OutDFile, srchstr,valposition, valformat, fixedval)
  }


#################################################################################
##################         End of Calibration         ###########################
#################################################################################

  ############## Read Output files #########################################

  ######### streamflow output
  library(Hmisc)

  setwd(wamisdir)
  oletsub = 6
  wlstnnm = "Younggok"
  wlstnid = "2008690"

  wamis = myGetWamisFlow(wlstnid, sdate2, edate)
  obs = wamis[,c(4,6)]
  colnames(obs) = c("date", "obs_cms")

  outrch = mySwatRchSummary(outdir, oletsub, sdate2, "water", "mean", sdate2, edate2,)
  sim = outrch$data[,4:5]
  colnames(sim) = c("date", "sim_cms")
  area = outrch$area

  data = merge(obs, sim, all=T)
  data = na.omit(data)

  imsi = myfilldate(data)
  mdata = aggregate(cbind(obs_cms, sim_cms) ~ yearmon, data=imsi, FUN=mean)

  pngname = paste(wlstnnm, "-", wlstnid, sep="")
  maintitle = pngname

  results = mySwatSummaryGraphTable(outdir, pngname, maintitle, data, mdata)


  ######### water quality output #######################################################
  wqoletsub = 2
  wqstnnm = "Wecheon-B"
  wqstnid = "��õB"

  outrch = mySwatRchSummary(outdir, wqoletsub, sdate2, "water", "mean", sdate2, edate2,)
  WSArea = outrch$area

  # get observed data
  tmdl = myGetTmdlWQuality(wqstnid, sdate2, edate)
  obs_flow = tmdl[c("date", "flow")]

  obs_ssc = tmdl[c("date", "ss")]
  #unit conversion: mg/l --> ton
  obs_ssc$ss = (obs_ssc$ss * obs_flow$flow * 60 * 60 * 24) / 10^6
  colnames(obs_ssc) = c("date", "Obs.SS (ton)")

  obs_tnc = tmdl[c("date", "tn")]
  #unit conversion: mg/l --> kg
  obs_tnc$tn = (obs_tnc$tn * obs_flow$flow * 60 * 60 * 24) / 10^3
  colnames(obs_tnc) = c("date", "Obs. TN (kg)")

  obs_tpc = tmdl[c("date", "tp")]
  #unit conversion: mg/l --> kg
  obs_tpc$tp = (obs_tpc$tp * obs_flow$flow * 60 * 60 * 24) / 10^3
  colnames(obs_tpc) = c("date", "Obs. TP (kg)")

  #unit conversion: cms --> mm
  obs_flow$flow = (obs_flow$flow * 60 * 60 * 24) / (WSArea * 10^3)
  colnames(obs_flow) = c("date", "Obs. flow (mm)")


  # get simulated data
  outrch = mySwatRchSummary(outdir, wqoletsub, sdate2, "waterd", "mean", sdate2, edate2,)
  sim_flow = outrch$data[c("date", "flow_mm")]
  colnames(sim_flow) = c("date", "Sim. flow (mm)")
  outrch = mySwatRchSummary(outdir, wqoletsub, sdate2, "sed", "mean", sdate2, edate2,)
  sim_ssc = outrch$data[c("date", "Sed_ton")]
  colnames(sim_ssc) = c("date", "Sim. SS (ton)")
  outrch = mySwatRchSummary(outdir, wqoletsub, sdate2, "tn", "mean", sdate2, edate2,)
  sim_tnc = outrch$data[c("date", "TN_kg")]
  colnames(sim_tnc) = c("date", "Sim. TN (kg)")
  outrch = mySwatRchSummary(outdir, wqoletsub, sdate2, "tp", "mean", sdate2, edate2,)
  sim_tpc = outrch$data[c("date", "TP_kg")]
  colnames(sim_tpc) = c("date", "Sim. TP (kg)")


  setwd(outdir)
  data_flow = merge(obs_flow, sim_flow, all=T)
  data_ss = merge(obs_ssc, sim_ssc, all=T)
  data_tnc = merge(obs_tnc, sim_tnc, all=T)
  data_tpc = merge(obs_tpc, sim_tpc, all=T)

  # data for time-series
  data_all = data_flow
  data_all = cbind(data_all, data_ss[2:3], data_tnc[2:3] , data_tpc[2:3])
  fname = paste(wqstnid, "-TimeSeries.csv", sep="")
  write.csv(data_all, fname, row.names=F, na="")

  # data for scatter plot
  data_all = na.omit(data_all)
  fname = paste(wqstnid, "-ScatterPlot.csv", sep="")
  write.csv(data_all, fname, row.names=F, na="")

}

##########################################################################################
############################?ó????? Run  ##############################

# Create Station List DBF(DBF????�� ??��?? ?? ??? CSV?? ???????? ?ٲٰ? R???? DBF?? ???)
dbfdir = "F:/SWAT-NakDong/2008/Scenarios/Default/Forecast/Observed/SWATDB"
flist = list.files(dbfdir, pattern = glob2rx("*.csv"), full.names = F)
fcnt = length(flist)
for(i in 1:fcnt){
  setwd(dbfdir)
  infile = flist[i]
  outfile = paste(substr(infile,1,nchar(infile)-4), ".dbf", sep="")
  data = read.csv(infile, header=T)
  write.dbf(data, outfile)
}

# Create Weather DBF files based on forecast data
source("F:/F-CMIP5-V3.0.R")
indir = "F:/SWAT-NakDong/2008/Scenarios/Default/Forecast/Scenario/SWATDB"
flist = list.files(indir, pattern = glob2rx("*.csv"), full.names = F)
fcnt = length(flist)
for(i in 1:fcnt){
  fname = flist[i]
  DataDFile = paste(indir, "/", fname, sep="")
  data = read.csv(DataDFile, header=T)
  stndata = data[c("date", "prcp", "tmax", "tmin", "wspd", "rhum", "rsds")]
  stndata = filldate(stndata)
  stnid = substr(fname, 1, nchar(fname)-4)
  swat = CMIP5.Daily.WriteDBF4SWAT(stndata, stnid, indir)
}


# Run SWAT manually
swatdbdir = "F:/SWATDB"
exedir = paste(swatdbdir, "/0_code-exe", sep="")
simdir = "F:/SWAT-NakDong/2008/Scenarios/Default/Forecast/TxtInOut"
caldir = "F:/SWAT-NakDong/2008/Scenarios/Default/CalInOut"
inputdir = "F:/SWAT-NakDong/2008/Scenarios/Default/UserInput"

############## ?????? ��?? ####################################################
#outdir = "F:/SWAT-NakDong/2008/Scenarios/Default/Forecast/Observed"
outdir = "F:/SWAT-NakDong/2008/Scenarios/Default/Forecast/Scenario"

# Copy weather input files from SWATDB into TxtInOut folder

# CIO fiel is created using interface: fixed (1988~2011)

# copy calibrated files(gw, hru, mgt, rte, sol, sub, swq of Calibration folder) manually

#################copy bsn file#################################################
calfile = paste(caldir, "/calibration.bsn", sep="")
outfile = paste(simdir, "/basins.bsn", sep="")
file.copy(calfile, outfile, overwrite=T)


# Copy output files to scenario directory
setwd(simdir)
system("cmd.exe /c swat591-64.exe")


# Copy output files to scenario directory
temp = myCopySwatInputOutputFiles_Local(inputdir, outdir, simdir, "Output")

# extraxt output file(rch)
oletsub = 11 #?ְ?õ
oletsub = 2 #��õB
sdate = "1988-01-01"
sdate2 = "1990-01-01"
edate = "2011-12-31"
edate2 = edate

outrch = mySwatRchSummary(outdir, oletsub, sdate2, "water", "mean", sdate2, edate2,)
sim = outrch$data[,4:5]
colnames(sim) = c("date", "flow_cms")
setwd(outdir)
write.csv(sim, "Output-flow.csv", row.names=F)

outrch = mySwatRchSummary(outdir, oletsub, sdate2, "sedc", "mean", sdate2, edate2,)
sim = outrch$data[c("date", "Sed_mgl")]
colnames(sim) = c("date", "SS_mg/l")
write.csv(sim, "Output-SS.csv", row.names=F)


outrch = mySwatRchSummary(outdir, oletsub, sdate2, "tnc", "mean", sdate2, edate2,)
sim = outrch$data[c("date", "TN_mgl")]
colnames(sim) = c("date", "TN_mg/l")
write.csv(sim, "Output-TN.csv", row.names=F)


outrch = mySwatRchSummary(outdir, oletsub, sdate2, "tpc", "mean", sdate2, edate2,)
sim = outrch$data[c("date", "TP_mgl")]
colnames(sim) = c("date", "TP_mg/l")
write.csv(sim, "Output-TP.csv", row.names=F)


#################################################################################
#################################################################################
# calculate monthly mean concentration and get min, max values
wdir = "D:/2014-WQForecast/424_WQuality/SWAT-Observed"
files = list.files(wdir, pattern = glob2rx("Output*.csv"), full.names=F)
fcnt = length(files)

# for(i in 1:fcnt){
#   file = files[i]
#   WqDFile =  paste(wdir, "/", file, sep="")
#   if(i == 1) {
#     data = read.csv(WqDFile, header = T)
#   } else {
#     temp = read.csv(WqDFile, header = T)
#     data = merge(data, temp, by = "date")
#   }
# }
#
#
# colnames(data) = c("date", "flow", "ss", "tn", "tp")
# data$ss2 = data$flow * data$ss
# data$tn2 = data$flow * data$tn
# data$tp2 = data$flow * data$tp
#
# data = myfilldate(data)
# avgdata = aggregate(cbind(flow, ss, tn, tp) ~ yearmon, data = data, FUN = mean, na.rm=T)
# mindata = aggregate(cbind(flow, ss, tn, tp) ~ yearmon, data = data, FUN = min, na.rm=T)
# maxdata = aggregate(cbind(flow, ss, tn, tp) ~ yearmon, data = data, FUN = max, na.rm=T)
#
# setwd(wdir)
# write.csv(avgdata, "Monthly-mean.csv", row.names=F)
# write.csv(mindata, "Monthly-min.csv", row.names=F)
# write.csv(maxdata, "Monthly-max.csv", row.names=F)
#
# obs = read.csv("D:/2014-WQForecast/424_WQuality/Observed-WQ/?ְ?õ-stream_wquality.csv")
# obs = obs[c("yearmon", "SS", "TN", "TP")]
#
# cdata = merge(avgdata, obs, by="yearmon", all=T)
# write.csv(cdata, "Monthly-combined.csv", row.names=F)
obsdata = read.csv("D:/2014-WQForecast/424_WQuality/Observed-WQ/?ְ?õ-stream_wquality.csv")
obsdata = read.csv("D:/2014-WQForecast/424_WQuality/Observed-WQ/��õB-tmdl_wquality.csv")


#file = "Output-SS.csv"
#obsdata = obs[c("yearmon", "SS")]
#outfile = "Monthly-Combined-SS.csv"

# file = "Output-TN.csv"
# obsdata = obs[c("yearmon", "TN")]
# outfile = "Monthly-Combined-TN.csv"

file = "Output-TP.csv"
obsdata = obs[c("yearmon", "TP")]
outfile = "Monthly-Combined-TP.csv"


WqDFile =  paste(wdir, "/", file, sep="")
data = read.csv(WqDFile, header = T)


colnames(data) = c("date", "conc")

data = myfilldate(data)
avgdata = aggregate(conc ~ yearmon, data = data, FUN = mean, na.rm=T)
mindata = aggregate(conc ~ yearmon, data = data, FUN = min, na.rm=T)
maxdata = aggregate(conc ~ yearmon, data = data, FUN = max, na.rm=T)

outdata = merge(avgdata, mindata, by="yearmon", all=T)
outdata = merge(outdata, maxdata, by="yearmon", all=T)


outdata = merge(outdata, obsdata, by="yearmon", all=T)

colnames(outdata) = c("yearmon", "mean", "min", "max", "obs")
setwd(wdir)
write.csv(outdata, outfile, row.names=F)

#################################################################################################
##### ��õB

#################################################################################
#################################################################################
# calculate monthly mean concentration and get min, max values
options(stringsAsFactors = FALSE)

for(j in 1:2){
  if(j == 1){
    wdir = "D:/2014-WQForecast/424_WQuality/��õB/SWAT-Observed"
  } else {
    wdir = "D:/2014-WQForecast/424_WQuality/��õB/SWAT-Forecast"
  }

  files = list.files(wdir, pattern = glob2rx("Output*.csv"), full.names=F)
  fcnt = length(files)

  for(i in 1:fcnt){
    file = files[i]
    WqDFile =  paste(wdir, "/", file, sep="")
    if(i == 1) {
      data = read.csv(WqDFile, header = T)
    } else {
      temp = read.csv(WqDFile, header = T)
      data = merge(data, temp, by = "date")
    }
  }


  colnames(data) = c("date", "flow", "ss", "tn", "tp")

  data = myfilldate(data)
  avgdata = aggregate(cbind(ss, tn, tp) ~ yearmon, data = data, FUN = mean, na.rm=T)
  mindata = aggregate(cbind(ss, tn, tp) ~ yearmon, data = data, FUN = min, na.rm=T)
  maxdata = aggregate(cbind(ss, tn, tp) ~ yearmon, data = data, FUN = max, na.rm=T)

  #weighted average
  yms = unique(data$yearmon)
  ymcnt = length(yms)
  for(k in 1:ymcnt){
    cym = yms[k]
    imsi = na.omit(data[which(data$yearmon == cym),])
    ss = with(imsi, sum(flow*ss)/sum(flow))
    tn = with(imsi, sum(flow*tn)/sum(flow))
    tp = with(imsi, sum(flow*tp)/sum(flow))
    if(k == 1){
      meandata = cbind(cym, ss, tn, tp)
    } else {
      tempdata = cbind(cym, ss, tn, tp)
      meandata = rbind(meandata, tempdata)
    }
  }

  setwd(wdir)
  write.csv(avgdata, "Monthly-smean.csv", row.names=F)
  write.csv(meandata, "Monthly-wmean.csv", row.names=F)
  write.csv(mindata, "Monthly-min.csv", row.names=F)
  write.csv(maxdata, "Monthly-max.csv", row.names=F)

}

##### Observed data
wdir = "D:/2014-WQForecast/424_WQuality/��õB/Observed-WQ"
data = read.csv("D:/2014-WQForecast/424_WQuality/��õB/Observed-WQ/��õB-tmdl_wquality.csv", header=T)
data$date = as.Date(with(data, paste(substr(date,1,4), "-", substr(date,6,7), "-", substr(date,9,10), sep="")))
data = data[c("date", "flow", "ss", "tn", "tp")]

data = myfilldate(data)
avgdata = aggregate(cbind(ss, tn, tp) ~ yearmon, data = data, FUN = mean, na.rm=T)
mindata = aggregate(cbind(ss, tn, tp) ~ yearmon, data = data, FUN = min, na.rm=T)
maxdata = aggregate(cbind(ss, tn, tp) ~ yearmon, data = data, FUN = max, na.rm=T)

#weighted average
yms = unique(data$yearmon)
ymcnt = length(yms)
for(k in 1:ymcnt){
  cym = yms[k]
  imsi = na.omit(data[which(data$yearmon == cym),])
  ss = with(imsi, sum(flow*ss)/sum(flow))
  tn = with(imsi, sum(flow*tn)/sum(flow))
  tp = with(imsi, sum(flow*tp)/sum(flow))
  if(k == 1){
    meandata = cbind(cym, ss, tn, tp)
  } else {
    tempdata = cbind(cym, ss, tn, tp)
    meandata = rbind(meandata, tempdata)
  }
}

setwd(wdir)
write.csv(avgdata, "Monthly-smean.csv", row.names=F)
write.csv(meandata, "Monthly-wmean.csv", row.names=F)
write.csv(mindata, "Monthly-min.csv", row.names=F)
write.csv(maxdata, "Monthly-max.csv", row.names=F)

#################################################################################
###### Combine into one files
syear = 2003
eyear = 2011

homedir = "D:/2014-WQForecast/424_WQuality/��õB"
dirs = c("Observed-WQ", "SWAT-Observed", "SWAT-Forecast")
dnms = c("OB", "SO", "SF")
files = c("Monthly-wmean.csv", "Monthly-min.csv", "Monthly-max.csv")
fnms = c("avg", "min", "max")
wqnms = c("ss", "tn", "tp")

dircnt = length(dirs); fcnt = length(files)

cnt = 1

for(i in 1:dircnt){
  dir = dirs[i]; dnm = dnms[i]
  wdir = paste(homedir, "/", dir, sep="")

  for(j in 1:fcnt){
    file = files[j]; fnm = fnms[j]
    DataDFile = paste(wdir, "/", file, sep="")

    if(cnt == 1){
      data = read.csv(DataDFile, header=T)
      colnames(data) = c("yearmon", paste(dnm, "-", fnm, "-ss", sep=""), paste(dnm, "-", fnm, "-tn", sep=""), paste(dnm, "-", fnm, "-tp", sep=""))
      cnt = cnt + 1
    } else {
      temp = read.csv(DataDFile, header=T)
      colnames(temp) = c("yearmon", paste(dnm, "-", fnm, "-ss", sep=""), paste(dnm, "-", fnm, "-tn", sep=""), paste(dnm, "-", fnm, "-tp", sep=""))
      data = merge(data, temp, by="yearmon", all=T)
      cnt = cnt + 1
    }

  }
}

data = data[which(substr(data$yearmon,1,4)>=syear & substr(data$yearmon,1,4)<=eyear),]
data$year = substr(data$yearmon,1,4)
data$month = as.numeric(substr(data$yearmon,6,7))
data = data[order(data$month, data$year),]

setwd(homedir)
for(k in 1:length(wqnms)){
  wqnm = wqnms[k]
  wqdata = data[,c(1, grep(wqnm, names(data)))]
  fname = paste("combined-summary-", wqnm, ".csv", sep="")
  write.csv(wqdata, fname, row.names=F)

}



