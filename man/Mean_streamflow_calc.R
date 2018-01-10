wdir = "D:/SWAT_Waras_CChange/LALO/CChange/Analysis"
setwd (wdir)
scnnms = c("historical", "rcp45", "rcp85")


for(i in 1:length(scnnms)){
  scnnm <- scnnms[i]

  mdlnms <- list.dirs(wdir, recursive = F, full.names = F)
  for(j in 1:length(mdlnms)){
    mdlnm <- mdlnms[j]
    curdir <- file.path(wdir, mdlnm)

    fname <- list.files(curdir, pattern = glob2rx(paste("*", scnnm, "-daily.csv", sep="")), full.names = T)
    flow <- read.csv(fname, header = T)
    colnames(flow) <- c("date", mdlnm)

    if(j == 1){
      out <- flow
    } else {
      temp <- flow
      out <- merge(out, temp, all = T)
    }
  }

  date <- out[1]
  val <- out[2:ncol(out)]
  mme <- as.data.frame(rowMeans(val))
  colnames(mme) <- c("MME")


  output <- cbind(date, val, mme)
  output$mon <- substr(output$date, 6, 7)
  output <- output[,2:ncol(output)]
  output <- aggregate(.~mon, data = output, FUN =mean)

  OutFname <- file.path(wdir, paste("Monthly_Avg_Flow_", scnnm, ".csv", sep=""))

  write.csv(output,OutFname, row.names = F)

}
