library(reshape)
library(foreign)

swatdbdir = "F:/SWATDB"
prjdir = "D:/SWAT-SMG/SWAT-WQ-Mankyong"
inputdir = paste(prjdir, "/Scenarios/sub_summary", sep="")
outdir = paste(prjdir, "/Scenarios/sub_summary/summary", sep="")
wshednm = "MKWQ-Sub-"

################### 재현성 분석 #######################
scnnm = "historical"
mdlnms = mdlnms = c("125km", "bcc-csm1-1", "CanESM2", "GFDL-CM3", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "MIROC-ESM", "MIROC-ESM-CHEM")

# read observed data
setwd(inputdir)
fname = paste(wshednm, "Observed-observed.csv", sep="")
data_obs = read.csv(fname, header=T)

# get subbasin file
SUB = data_obs$SUB

# calculated historical MME 
mdlcnt = length(mdlnms)
for(j in 1:mdlcnt){
  
  setwd(inputdir)
  
  mdlnm = mdlnms[j]
  fname = paste(wshednm, mdlnm, "-", scnnm, ".csv", sep="")
  if(j == 1){
    data = read.csv(fname, header=T)
    data$models = mdlnm
  } else {
    imsi = read.csv(fname, header=T)
    imsi$models = mdlnm
    data = rbind(data, imsi)
  }
  
}

mdata = melt(data, id=c("SUB", "models"))

data_mme = cast(mdata, SUB~variable, mean)
# data_min = cast(mdata, SUB~variable, min)
# data_max = cast(mdata, SUB~variable, max)

colnum = ncol(data_mme)
chng = (data_mme[,2:colnum] - data_obs[,2:colnum]) / data_obs[,2:colnum] * 100
chng_mme = cbind(SUB, chng)

# chng = (data_min[,2:colnum] - data_obs[,2:colnum]) / data_obs[,2:colnum] * 100
# chng_min = cbind(SUB, chng)
# 
# chng = (data_max[,2:colnum] - data_obs[,2:colnum]) / data_obs[,2:colnum] * 100
# chng_max = cbind(SUB, chng)

setwd(outdir)
obsfname = "observed-Observed.dbf"
mmefname = "historical-MME_data.dbf"
chnfname = "historical-MME_Chng.dbf"
write.dbf(data_obs, obsfname)
write.dbf(data_mme, mmefname)
write.dbf(chng_mme, chnfname)


################### 불확실성 분석 #######################
scnnms = c("rcp45", "rcp85")

scncnt = length(scnnms)
for(i in 1:scncnt){
  
  scnnm = scnnms[i]
  
  ############## describe model names here
  if(scnnm == "rcp45"){mdlnms = c("125km", "CanESM2", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "MIROC-ESM", "MIROC-ESM-CHEM")}
  if(scnnm == "rcp85"){mdlnms = c("125km", "bcc-csm1-1", "CanESM2", "GFDL-CM3", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "MIROC-ESM", "MIROC-ESM-CHEM")}

  # read historical MME data
  setwd(outdir)
  fname = "historical-MME_data.dbf"
  data_hist = read.dbf(fname)
  SUB = data_hist$SUB
  
  mdlcnt = length(mdlnms)
  for(j in 1:mdlcnt){
    
    setwd(inputdir)
    
    mdlnm = mdlnms[j]
    fname = paste(wshednm, mdlnm, "-", scnnm, ".csv", sep="")
    if(j == 1){
      data = read.csv(fname, header=T)
      data$models = mdlnm
    } else {
      imsi = read.csv(fname, header=T)
      imsi$models = mdlnm
      data = rbind(data, imsi)
    }
    
  }
  
  mdata = melt(data, id=c("SUB", "models"))
  
  data_mme = cast(mdata, SUB~variable, mean)
  data_min = cast(mdata, SUB~variable, min)
  data_max = cast(mdata, SUB~variable, max)
  
  colnum = ncol(data_mme)
  chng = (data_mme[,2:colnum] - data_obs[,2:colnum]) / data_obs[,2:colnum] * 100
  chng_mme = cbind(SUB, chng)
  
  chng = (data_min[,2:colnum] - data_obs[,2:colnum]) / data_obs[,2:colnum] * 100
  chng_min = cbind(SUB, chng)
  
  chng = (data_max[,2:colnum] - data_obs[,2:colnum]) / data_obs[,2:colnum] * 100
  chng_max = cbind(SUB, chng)
  
  fmmedata = paste(scnnm, "-MME_data.dbf", sep="")
  fmmechng = paste(scnnm, "-MME_chng.dbf", sep="")
  fmindata = paste(scnnm, "-MIN_data.dbf", sep="")
  fminchng = paste(scnnm, "-MIN_chng.dbf", sep="")
  fmaxdata = paste(scnnm, "-MAX_data.dbf", sep="")
  fmaxchng = paste(scnnm, "-MAX_chng.dbf", sep="")
  
  setwd(outdir)
  write.dbf(data_mme, fmmedata)
  write.dbf(chng_mme, fmmechng)
  write.dbf(data_min, fmindata)
  write.dbf(chng_min, fminchng)
  write.dbf(data_max, fmaxdata)
  write.dbf(chng_max, fmaxchng)
  
}


############# 시간적 분포 ############################################
swatdbdir = "F:/SWATDB"
prjdir = "D:/SWAT-SMG/SWAT-WQ-Mankyong"
inputdir = paste(prjdir, "/Scenarios/sub_summary", sep="")
outdir = paste(prjdir, "/Scenarios/sub_summary/summary", sep="")
wshednm = "MKWQ-"

subnms = c(26, 72, 63, 69)
#varnms = c("WYLDmm", "SYLDtha", "tn", "tp")
varnms = c("WYLDmm", "SYLDtha", "tn", "tp")
scnnms = c("rcp45", "rcp85")


subcnt = length(subnms)
for(i in 1:subcnt){
  subnm = subnms[i]
  
  varcnt = length(varnms)
  for(j in 1:varcnt){
    varnm = varnms[j]
    
    ########################### Historical data ##############################
    # read observed data
    fname = paste(inputdir, "/", wshednm, format(subnm,width=3), "-Observed-observed.csv", sep="")
    data = read.csv(fname, header=T)
    data = data[c("month", varnm)]
    colnames(data) = c("month", "Observed")
    
    
    scnnm = "historical"
    mdlnms = c("125km", "bcc-csm1-1", "CanESM2", "GFDL-CM3", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "MIROC-ESM", "MIROC-ESM-CHEM")
    
    mdlcnt = length(mdlnms)
    for(ii in 1:mdlcnt){

      mdlnm = mdlnms[ii]
      fname = paste(inputdir, "/", wshednm, format(subnm,width=3), "-", mdlnm, "-", scnnm, ".csv", sep="")
      if(ii == 1){
        mdldat = read.csv(fname, header=T)
        mdldat = mdldat[c(varnm)]
        colnames(mdldat) = c(mdlnm)
      } else {
        imsi = read.csv(fname, header=T)
        imsi = imsi[c(varnm)]
        colnames(imsi) = c(mdlnm)
        mdldat = cbind(mdldat, imsi)
      }
    }
    mme = as.data.frame(rowMeans(mdldat))
    colnames(mme) = c("MME")
  
    hist_data = cbind(data, mdldat, mme)
    
    foname = paste(outdir, "/", subnm, "-", varnm, "-", scnnm, ".csv", sep="")
    write.csv(hist_data, foname, row.names=F)
    
    ######################## Scenario data ###################################
    
    # get historical data
    data = hist_data[c("month", "MME")]
    colnames(data) = c("month", "historical")
    
    scncnt = length(scnnms)
    for(k in 1:scncnt){
      scnnm = scnnms[k]
      
      if(scnnm == "rcp45"){mdlnms = c("125km", "CanESM2", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "MIROC-ESM", "MIROC-ESM-CHEM")}
      if(scnnm == "rcp85"){mdlnms = c("125km", "bcc-csm1-1", "CanESM2", "GFDL-CM3", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "MIROC-ESM", "MIROC-ESM-CHEM")}
      
      mdlcnt = length(mdlnms)
      for(ii in 1:mdlcnt){
        
        mdlnm = mdlnms[ii]
        fname = paste(inputdir, "/", wshednm, format(subnm,width=3), "-", mdlnm, "-", scnnm, ".csv", sep="")
        if(ii == 1){
          mdldat = read.csv(fname, header=T)
          mdldat = mdldat[c(varnm)]
          colnames(mdldat) = c(mdlnm)
        } else {
          imsi = read.csv(fname, header=T)
          imsi = imsi[c(varnm)]
          colnames(imsi) = c(mdlnm)
          mdldat = cbind(mdldat, imsi)
        }
      }
      mme = as.data.frame(rowMeans(mdldat))
      colnames(mme) = c("MME")
      
      rcp_data = cbind(data, mdldat, mme)
      
      foname = paste(outdir, "/", subnm, "-", varnm, "-", scnnm, ".csv", sep="")
      write.csv(rcp_data, foname, row.names=F)
    } #scenario Loop
    
    
  } # Variable loop
} # subbasin loop
  
