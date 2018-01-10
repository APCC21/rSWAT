###################################################################################################################
# outtype: 1-flow, 2-Sediment, 3-TN, 4-TP
# funtype : 1-sum, 2-mean, 3-max, 4-min, 5-sd, 6-length
###################################################################################################################
Swat.Time.Series.Graph <- function(data, tstep, isLine, yl) {

  library(Hmisc)

  data = na.omit(data)

  par(oma=c(2,0,1,1))
  par(mar=c(4,6,1,1))

  #yl = "streamflow (cms)"

  time = data[,1]
  obs = data[,2]
  sim = data[,3]

  ymax = max(obs, sim, na.rm=T)
  xcnt = length(time)
  xmax = length(time) + 1

  y = pretty(c(0,ymax))

  if(tstep == "daily") {
    x_year = seq(183,xcnt, by=365)
    x_year_label = substr(time[x_year],1,4)
    x_tick = c(0,seq(365,xcnt+1, by=365))
    x_tick_label = rep("", length(x_tick))
  }

  if(tstep == "monthly") {
    x_year = seq(6,xcnt, by=12)
    x_year_label = substr(time[x_year],1,4)
    x_tick = c(0,seq(12.5,xcnt+0.5, by=12))
    x_tick_label = rep("", length(x_tick))
  }

  yrange = range(y)
  xrange = c(0,xmax)

  ######### Time serise plot
  plot(sim, type='n', ylim=yrange, xlim=xrange, xaxs='i',yaxs='i', xlab="", ylab="", xaxt='n',yaxt='n')

  ##### draw obs data
  if(isLine == "yes"){
    lines(obs, type='b', pch=1, lty=1)
  }
  if(isLine == "no"){
    polygon(c(1:xcnt,rev(1:xcnt)),c(obs,rep(0,xcnt)),col='gray',border=NA)
  }

  #### draw simulated data
  if(tstep == "daily") {
    lines(sim, type='l', pch=20, lty=3)
    if(isLine == "yes"){
      legend('top',inset=0.01,legend=c("observed", "simulated"),pch=c(NA,NA),lty=c(1,3),col=c('black','black'),lwd=c(1,1), ncol=2,bty='n')
      title(ylab=yl)
    } else {
      legend('top',inset=0.01,legend=c("observed", "simulated"),pch=c(NA,NA),lty=c(1,1),col=c('gray','black'),lwd=c(5,1), ncol=2,bty='n')
      title(ylab=yl)
    }

  }

  if(tstep == "monthly") {
    lines(sim, type='b', pch=20, lty=3)
    #minor.tick(nx=10, tick.ratio=0.4)
    if(isLine == "yes"){
      legend('top',inset=0.01,legend=c("observed", "simulated"),pch=c(1,20),lty=c(1,3),col=c('black','black'),lwd=c(1,1), ncol=2,bty='n')
      title(ylab=yl)
    } else {
      legend('top',inset=0.01,legend=c("observed", "simulated"),pch=c(NA,20),lty=c(1,3),col=c('gray','black'),lwd=c(5,1), ncol=2,bty='n')
      title(ylab=yl)
    }

  }

  axis(2, at=y,labels=y, las=2)
  axis(1, at=x_tick, labels=x_tick_label)
  axis(1, at=x_year, labels=x_year_label, tck=0)

  return(yl)

}


###################################################################################################################
# outtype: 1-flow, 2-Sediment, 3-TN, 4-TP
# funtype : 1-sum, 2-mean, 3-max, 4-min, 5-sd, 6-length
###################################################################################################################
Swat.Scatter.Plot.Graph <- function(data, perr) {

  library(Hmisc)

  par(oma=c(2,0,2,1))
  #par(mar=c(7,12,3,9))
  par(mar=c(8.5, 6, 1, 1))

  obs = data[,2]
  sim = data[,3]

  ymax = max(obs, sim, na.rm=T)
  y = pretty(c(0,ymax))
  yrange = range(y)

  ls = lm(sim~obs)
  a = ls$coef[2]
  b = ls$coef[1]
  r2 = summary(ls)[[8]]
  fit = ls$fit
  # get start and end point of regression line
  minmax = (obs==min(obs, na.rm=T) | obs==max(obs, na.rm=T))
  # position of equation and R2
  xloc = y[2]/3
  yloc = (y[length(y)] + y[length(y)-1])/2
  yloc2 =  y[length(y)-1] + (y[length(y)] - y[length(y)-1])/10


  plot(yrange, yrange, type='l', lty=2, ylim=yrange, xlim=yrange, xaxs='i',yaxs='i', xlab="", ylab="", xaxt='n',yaxt='n')
  points(obs, sim, pch=1,cex=1.2)
  axis(2, at=y,labels=y, las=2)
  axis(1, at=y, labels=y, las=0)
  lines(obs[minmax],fit[minmax],lwd=3)
  text(xloc, yloc, pos=4, bquote(y ~ "=" ~ .(round(a,3))*x+.(round(b,3))))
  text(xloc, yloc2, pos=4, bquote(R^2 ~ "=" ~ .(round(r2,4))))
  title(xlab="observed streamflow (cms)", ylab="simulated streamflow (cms)")

  out= Swat.Calc.Error(data)
  nse = out$nse
  error = out$error
  mse = out$mse
  rmse = out$rmse
  rsr = out$rsr
  nof = out$nof

  mtext(bquote("NSE=" ~ .(round(nse,2))), side=1, line=4.5)
  mtext(bquote("RMSE=" ~ .(round(rmse,1))), side=1, line=5.5)
  mtext(bquote("RSR=" ~ .(round(rsr,1))), side=1, line=6.5)
  if(perr == "yes"){
    mtext(bquote("% Error=" ~ .(round(error,1))), side=1, line=7.5)
  }


  return(r2)

}

###################################################################################################################
# Copy necessary input files into simulation folder
###################################################################################################################
Swat.Summary.Graph.Table <- function(outdir, pngname, maintitle, ddata, mdata) {

  library(png)
  library(Hmisc)

  setwd(outdir)

  pngfilename = paste(pngname, ".png", sep="")
  dayfilename = paste(pngname, "-daily.csv", sep="")
  monfilename = paste(pngname, "-monthly.csv", sep="")

  #setup device
  png(pngfilename, width = 8, height = 11, units = 'in', res = 400)

  #Split screen and put the plots
  #Ref: http://stackoverflow.com/questions/17553138/
  close.screen( all = TRUE )
  split.screen(matrix(c(0,0,0,  1,1,1,  0,0.42,0.69, 0.42,0.69,0.96), ncol=4))
  split.screen(matrix(c(0,0.5,  0.5,1,  0,0,  1,1), ncol=4), screen = 1 )
  screen( 3 )
  if(!is.null(ddata)){
    daily = Swat.Time.Series.Graph(ddata, "daily", "no", "streamflow (cms)")
  }
  screen( 2 )
  monthly = Swat.Time.Series.Graph(mdata, "monthly", "no", "streamflow (cms)")
  screen( 4 )
  if(!is.null(ddata)){
    daily = Swat.Scatter.Plot.Graph(ddata, "yes")
  }
  screen( 5 )
  monthly = Swat.Scatter.Plot.Graph(mdata, "no")
  title(main=maintitle, outer=T)

  if(!is.null(ddata)){
    write.csv(ddata, dayfilename, row.names=F)
  }
  write.csv(mdata, monfilename, row.names=F)

  dev.off()

  return(mdata)

}

