source("F:/Functions-Observations.R")
# source("F:/Functions-System.R")
# source("F:/Functions-SWAT.R")
# source("F:/Functions-GIS.R")
# source("F:/Functions-AgReservoirs.R")
# source("F:/Functions-CMIP5.R")

######## Extract Stream Level data 
stndir = "D:/2014-WQForecast/Data"
stnfile = "WL_Stations.csv"

setwd(stndir)

stndata = read.csv(stnfile, header = T)
stncode = stndata$Code
stncnt = length(stncode)

for(i in 1:stncnt){
  wlstnid = stncode[i]
  wamis = myGetWamisFlow(wlstnid)
  #wamis = myGetWamisFlow(wlstnid, "2010-01-01", "2011-12-31")
  plot = myGraphWamisFlow(wamis)
}

######## Extract TMDL Water Quality data
stndir = "D:/2014-WQForecast/Data"
stnfile = "TMDL_WQ_Stations.csv"

setwd(stndir)

stndata = read.csv(stnfile, header = T)
stncode = stndata$Code
stncnt = length(stncode)

for(i in 1:stncnt){
  
  stnid = stncode[i]
  stnid = toString(stnid)
  tmdl = myGetTmdlWQuality(stnid)
  
}


######## Extract Stream Water Quality data
stndir = "D:/2014-WQForecast/ProjectDB"
stnfile = "ND-StreamWQStns-1990.csv"
dbdir = "F:/StationDB/wquality/stream"
dbfile ="Stream-WQ-NakDong-ALL_2014Mar.csv"

setwd(stndir)

stndata = read.csv(stnfile, header = T)
stnnms = as.matrix(stndata$OBSNM)
stncnt = length(stnnms)

for(i in 1:stncnt){
  
  stnnm = stnnms[i]
  wqual = Observations.GetStreamWQuality(dbdir, dbfile, stnnm) 
  
}

#===============================================================================
# Reshape data

library(reshape)
library(doBy)
library(ggplot2)

stndir = "D:/2014-WQForecast/Data"
stnfile = "ND-StreamWQStns-1990.csv"
dbdir = "F:/StationDB/wquality/stream"
dbfile ="Stream-WQ-NakDong-ALL_2014Mar.csv"

#******************************************************
Local.add.period.column <- function(x){
  if(x<=1990)
    return(0)
  if(x>=1991 & x<=1995)
    return(1)
  if(x>=1996 & x<=2000)
    return(2)
  if(x>=2001 & x<=2005)
    return(3)
  if(x>=2006 & x<=2010)
    return(4)
  else
    return(5)
}
#****************************************************

#******************************************************
Local.add.season.column <- function(x){
  if(x>=3 & x<=5)
    return(1)
  if(x>=6 & x<=8)
    return(2)
  if(x>=9 & x<=11)
    return(3)
  else
    return(4)
}
#****************************************************



setwd(stndir)
stndata = read.csv(stnfile, header = T)
stnnms = as.matrix(stndata$OBSNM)
stncnt = length(stnnms)

for(i in 1:stncnt){
  
  stnnm = stnnms[i]
  wqual = Observations.GetStreamWQuality(dbdir, dbfile, stnnm)
  wqual = wqual[c("stnnm", "yearmon", "year", "month", "Temp", "BOD", "SS", "TN", "TP", "Chla")]
  wqual$period = sapply(wqual$year, Local.add.period.column)
  wqual$season = sapply(wqual$month, Local.add.season.column)
  
  bsndata = stndata[c("OBSNM", "BSNCD")]
  colnames(bsndata) = c("stnnm", "sbsnnm")
  wqual = merge(wqual, bsndata, by="stnnm")
  wqual$mbsnnm = substr(wqual$sbsnnm, 1, 4)
  
  if(i == 1){
    mdata = melt(wqual, id=c("stnnm", "yearmon", "year", "month", "period", "mbsnnm", "sbsnnm", "season"))
  } else {
    imsi = melt(wqual, id=c("stnnm", "yearmon", "year", "month", "period", "mbsnnm", "sbsnnm", "season"))
    mdata = rbind(mdata, imsi)
  }
}

mdata$prdssn = paste(mdata$period, "-", mdata$season, sep="")

outdata = summaryBy(value~variable+stnnm+yearmon, data=mdata, FUN = mean, na.rm=T)
meandata = reshape(outdata, idvar = c("variable", "yearmon"), timevar = "stnnm", direction = "wide")
colnames(meandata) = c("yearmon", "variable", stnnms)
setwd(stndir)
write.csv(meandata, "StreamWQ-Yearmon-output.csv", row.names=F)

outdata = summaryBy(value~variable+stnnm+period, data=mdata, FUN = mean, na.rm=T)
meandata = reshape(outdata, idvar = c("variable", "period"), timevar = "stnnm", direction = "wide")
colnames(meandata) = c("period", "variable", stnnms)
setwd(stndir)
write.csv(meandata, "StreamWQ-Period-output.csv", row.names=F)

outdata = summaryBy(value~variable+stnnm+season, data=mdata, FUN = mean, na.rm=T)
meandata = reshape(outdata, idvar = c("variable", "season"), timevar = "stnnm", direction = "wide")
colnames(meandata) = c("season", "variable", stnnms)
setwd(stndir)
write.csv(meandata, "StreamWQ-Season-output.csv", row.names=F)

outdata = summaryBy(value~variable+stnnm+prdssn, data=mdata, FUN = mean, na.rm=T)
meandata = reshape(outdata, idvar = c("variable", "prdssn"), timevar = "stnnm", direction = "wide")
colnames(meandata) = c("prdssn", "variable", stnnms)
setwd(stndir)
write.csv(meandata, "StreamWQ-PrdSsn-output.csv", row.names=F)


################# Graph : not yet ###################################################

head(meandata)

setwd(stndir)
write.csv(meandata, "final-output.csv", row.names=F)


varcnt = length(varnms)
for(i in 1:varcnt){
  varnm = varnms[i]
  vardata = bsndata[which(bsndata$variable == varnm), c("scenario", "basin",  "model", "year", "value")]
  
  histmme = vardata[which(vardata$scenario == "historical"), ]
  histmme$model ="HIST-MME"
  
  rcp45mme = vardata[which(vardata$scenario == "rcp45"), ]
  rcp45mme$model ="RCP4.5-MME"
  
  rcp85mme = vardata[which(vardata$scenario == "rcp85"), ]
  rcp85mme$model ="RCP8.5-MME"
  
  vardata = rbind(vardata, histmme, rcp45mme, rcp85mme)
  
  vardata$basin = factor(vardata$basin, levels = bsnnms) 
  vardata$scenario = factor(vardata$scenario, levels = scnnms)
  
  vardata$model = factor(vardata$model, levels = c("Observed", "125km", "CanESM2", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "MIROC-ESM", "MIROC-ESM-CHEM", "HIST-MME", "RCP4.5-MME", "RCP8.5-MME"))
  
  #   setwd(cmipdir)
  #   fname = paste(varnm, ".png", sep="")
  #   png(fname, width = 8, height = 6, units = 'in', res = 600)
  
  ggplot() + facet_grid(basin~scenario, scales="free", space="free_x", labeller=plot_labeller) +
    geom_boxplot(data=vardata, aes(x=model, y=value)) +
    #coord_cartesian(ylim = c(0,11)) +
    theme(axis.text.x=element_text(angle = 90, vjust = 0, hjust=1, colour="black"),
          axis.text.y=element_text(colour="black"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  #   dev.off()
}
