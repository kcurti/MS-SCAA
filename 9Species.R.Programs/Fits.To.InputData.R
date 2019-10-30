###########################################################
#Plot observed input data and predicted fits

###########################################################

#Figure height and widths for figs of par(mfcol=c(3,3))
#fig.ht.3 <- 6.8 # Original - with middle x-axes
fig.ht.3 <- 6.2 # Middle x-axes removed
fig.wdth.3 <- 6.3
#Figure height and widths for figs of par(mfcol=c(2,3))
fig.ht.2 <- 4.35 # Middle x-axes removed
fig.wdth.2 <- 6.3

library(TeachingDemos)  #For figure legend
type.key <- c('a)' , 'b)' , 'c)')
key.x.coord <- -0.45
key.y.coord <- 1.11

lty.key <- 1:6
ssp.lty <- 2
msp.lty <- 1

ssp.col <- 'black'
msp.col='black'

#line/point details
l.width <- 1.5
p.size <- 1.5
p.width <- 0.8
a.width <- 1
a.size <- 1.0

label.size <- 0.8
fig.name.cex <- 0.8

# fig.type <- 'wmf'


###########################################################
##### TotC
setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
if(fig.type == 'wmf') {win.metafile("./TotC_AllSp.wmf" , height = fig.ht.3, width = fig.wdth.3)}
par(mfrow=c(3,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
for (i in 1:nsp)  {
  par(las = 1) # horizontal text
  ytmp<-c(obs.totc[[i]],pred.totc[[i]])
    plot(yrs[i,],obs.totc[[i]],type="p",xlab="",ylab="",xlim=yr.lim,ylim=c(0,max(ytmp)),axes=F,cex=p.size,lwd=p.width)
    lines(yrs[i,],pred.totc[[i]],col=msp.col,lwd=l.width, lty = lty.key[msp.lty])
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
    axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
    if(i >= 7)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
  mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
  box(lwd=a.width)
  } # end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Thousands of metric tons", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}



###########################################################
##### TotFIC

for (j in 1:sp9.rep$nFIC)  {
  setwd(fig.dir)
  if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
  if(fig.type == 'wmf') {win.metafile( paste("./TotFIC_AllSp",j,"wmf",sep=".") , height = fig.ht.3, width = fig.wdth.3)}
  par(mfrow=c(3,3))
  par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
  for (i in 1:nsp)  {
    par(las = 1)
    if (TSwt[i,j] == 0)  {
      # leave empty slot
      plot(yrs[i,],rep(1,length(yrs[i,])),type="l",col=par("bg"),axes=F,xlab="",ylab="") }
    else {
      obs.tmp <- obs.totfic[[i]][j,]
      pred.tmp <- pred.totfic[[i]][j,]
      ytmp<-c(obs.tmp,pred.tmp)
      plot  (yrs[i,],obs.tmp,type="p",xlab="",ylab="",xlim=yr.lim,ylim=c(0,max(ytmp)),axes=F,cex=p.size,lwd=p.width)
      lines (yrs[i,],pred.tmp,col=msp.col,lwd=l.width, lty = lty.key[msp.lty])
      axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
      axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
      box(lwd=a.width)
      mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
      }
    if(i >= 7)  {axis(side=1, at=yr.ticks, labels=TRUE, col='black', cex.axis=a.size,lwd=a.width)}
    }  # end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Number/tow", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}
  }  # end of survey loop



###########################################################
##### Plot age proportions

is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#####Cprop
for (i in 3:nsp)
  {  
  setwd(fig.dir)
  if(fig.type == 'gui')    {dev.new(height = fig.ht.3+3, width = fig.wdth.3)}
  if(fig.type == 'wmf') {win.metafile( paste("./Cprop",sp.names[i],"wmf",sep=".") , height = fig.ht.3+3, width = fig.wdth.3)}
  par(mfcol=c(ceiling(nage[i]/2),2))
  par(mar=c(2, 2.5, 0.3, 1.0) +0.1);  par(oma=c(2.5,2.0,2.0,0)) # Horizontal y-axis
  for (a in 1:nage[i])
    {
    par(las = 1)
    ytmp <- c(obs.cprop[[i]][,a],pred.cprop[[i]][,a])
    plot(yrs[i,],obs.cprop[[i]][,a],type="p",xlab="",ylab="",ylim=c(0,max(ytmp,na.rm=T)),axes=F)
    lines(yrs[i,],pred.cprop[[i]][,a])
    axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=0.95)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.95)
    box()
    mtext(paste("Age",a), side=3, cex=fig.name.cex, line=0.3, font=2)
    if(is.wholenumber(a /par()$mfrow[1]) == TRUE)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
    }  #end of age loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Proportion at age", side=2, line=0.7, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}
  } # end of species loop
# For spiny dogfish and winter skate, split props into two figures
for (i in 1:2)
  {  
  fage.tmp <- floor(c(1,(1+(nage[i]/2))))
	lage.tmp <- floor(c(nage[i]/2,nage[i]))
  for (j in 1:length(lage.tmp))  {
    setwd(fig.dir)
    if(fig.type == 'gui')    {dev.new(height = fig.ht.3+3, width = fig.wdth.3)}
    if(fig.type == 'wmf') {win.metafile( paste("./Cprop",sp.names[i],"ages",j,"wmf",sep=".") , height = fig.ht.3+3, width = fig.wdth.3)}
    par(mfcol=c(ceiling((lage.tmp[j]-fage.tmp[j]+1)/2),2))
    par(mar=c(2, 2.5, 0.3, 1.0) +0.1);  par(oma=c(2.5,2.0,2.0,0)) # Horizontal y-axis
    for (a in fage.tmp[j]:lage.tmp[j])
      {
      par(las = 1)
      ytmp <- c(obs.cprop[[i]][,a],pred.cprop[[i]][,a])
      plot(yrs[i,],obs.cprop[[i]][,a],type="p",xlab="",ylab="",ylim=c(0,max(ytmp,na.rm=T)),axes=F)
      lines(yrs[i,],pred.cprop[[i]][,a])
      axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=0.95)
      axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.95)
      box()
      mtext(paste("Age",a), side=3, cex=fig.name.cex, line=0.3, font=2)
      }  #end of age loop
    par(las = 0)
    mtext("Year",side=1,line=1,cex=label.size,outer=T)
    mtext("Proportion at age", side=2, line=0.7, cex=label.size,outer=T) 
    if (fig.type == 'wmf') {dev.off()}
    }  # end of lage.tmp loop
  } # end of species loop


######Sprop
for (k in 1:sp9.rep$nFIC)  
  {
  for (i in 3:nsp)
    { 
    if(SPwt[i,k] != 0)  
      {
      setwd(fig.dir)
      if(fig.type == 'gui')    {dev.new(height = fig.ht.3+3, width = fig.wdth.3)}
      if(fig.type == 'wmf') {win.metafile( paste("./Sprop",sp.names[i],"survey",k,"wmf",sep=".") , height = fig.ht.3+3, width = fig.wdth.3)}
      par(mfcol=c(ceiling(nage[i]/2),2))
      par(mar=c(2, 2.5, 0.3, 1.0) +0.1);  par(oma=c(2.5,2.0,2.0,0)) # Horizontal y-axis
      for (a in 1:nage[i])
        {
        par(las = 1)
        ytmp <- c(obs.sprop[[i]][[k]][,a],pred.sprop[[i]][[k]][,a])
        plot(yrs[i,],obs.sprop[[i]][[k]][,a],type="p",xlab="",ylab="",ylim=c(0,max(ytmp,na.rm=T)),axes=F)
        lines(yrs[i,],pred.sprop[[i]][[k]][,a])
        axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=0.95)
        axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.95)
        box()
        mtext(paste("Age",a), side=3, cex=fig.name.cex, line=0.3, font=2)
        if(is.wholenumber(a /par()$mfrow[1]) == TRUE)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
        }  #end of age loop
      par(las = 0)
      mtext("Year",side=1,line=1,cex=label.size,outer=T)
      mtext("Proportion at age", side=2, line=0.7, cex=label.size,outer=T) 
      if (fig.type == 'wmf') {dev.off()}
      } # end of SPwt If Statement
    } # end of species loop
  # For spiny dogfish and winter skate, split props into two figures
  for (i in 1:2)
    {
    if(SPwt[i,k] != 0)  
      {
      fage.tmp <- floor(c(1,(1+(nage[i]/2))))
  	  lage.tmp <- floor(c(nage[i]/2,nage[i]))
      for (j in 1:length(lage.tmp))  {
        setwd(fig.dir)
        if(fig.type == 'gui')    {dev.new(height = fig.ht.3+3, width = fig.wdth.3)}
        if(fig.type == 'wmf') {win.metafile( paste("./Sprop",sp.names[i],"survey",k,"ages",j,"wmf",sep=".") , height = fig.ht.3+3, width = fig.wdth.3)}
        par(mfcol=c(ceiling((lage.tmp[j]-fage.tmp[j]+1)/2),2))
        par(mar=c(2, 2.5, 0.3, 1.0) +0.1);  par(oma=c(2.5,2.0,2.0,0)) # Horizontal y-axis

        for (a in fage.tmp[j]:lage.tmp[j])
          {
          par(las = 1)
          ytmp <- c(obs.sprop[[i]][[k]][,a],pred.sprop[[i]][[k]][,a])
          plot(yrs[i,],obs.sprop[[i]][[k]][,a],type="p",xlab="",ylab="",ylim=c(0,max(ytmp,na.rm=T)),axes=F)
          lines(yrs[i,],pred.sprop[[i]][[k]][,a])
          axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=0.95)
          axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.95)
          box()
          mtext(paste("Age",a), side=3, cex=fig.name.cex, line=0.3, font=2)
          }  #end of age loop
        par(las = 0)
        mtext("Year",side=1,line=1,cex=label.size,outer=T)
        mtext("Proportion at age", side=2, line=0.7, cex=label.size,outer=T) 
        if (fig.type == 'wmf') {dev.off()}
        }  # end of lage.tmp loop
      } # end of SPwt If Statement
    } # end of species loop

  } # end of survey loop



###########################################################
##### Effective sample size
        # Calculated according to McAllister and Ianelli 1997, page 299

##### EfN.cprop
setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
if(fig.type == 'wmf') {win.metafile("./EfN_Cprop_AllSp.wmf" , height = fig.ht.3, width = fig.wdth.3)}
par(mfrow=c(3,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
for (i in 1:nsp)
  {
  par(las = 1)
    ytmp <- c(as.vector(EfN.cprop[[i]]),CPwt[i])
    plot(yrs[i,],EfN.cprop[[i]],type="p",xlab="",ylab="",ylim=c(0,max(ytmp,na.rm=T)),axes=F,cex=p.size,lwd=p.width)
    lines(yrs[i,],rep(CPwt[i],length(EfN.cprop[[i]])), lwd=l.width)
    axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
    box(lwd=a.width)
    mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
    if(i >= 7)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
  }  #end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Effective sample size", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}


##### EfN.sprop
for (j in 1:sp9.rep$nFIC)  {
  setwd(fig.dir)
  if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
  if(fig.type == 'wmf') {win.metafile( paste("./EfN_Sprop_AllSp",j,"wmf",sep=".") , height = fig.ht.3, width = fig.wdth.3)}
  par(mfrow=c(3,3))
  par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
  for (i in 1:nsp)  {
    par(las = 1)
    if (SPwt[i,j] == 0)  {
      # leave empty slot
      plot(yrs[i,],rep(1,length(yrs[i,])),type="l",col=par("bg"),axes=F,xlab="",ylab="") }
    else {
      ytmp<-c(EfN.sprop[[i]][[j]] , SPwt[i,j])
      plot(yrs[i,],EfN.sprop[[i]][[j]],type="p",xlab="",ylab="",ylim=c(0,max(ytmp,na.rm=T)),axes=F,cex=p.size,lwd=p.width)
      lines(yrs[i,],rep(SPwt[i,j],length(EfN.sprop[[i]][[j]])), lwd=l.width)
      axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
      axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
      box(lwd=a.width)
      mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
      }
    if(i >= 7)  {axis(side=1, at=yr.ticks, labels=TRUE, col='black', cex.axis=a.size,lwd=a.width)}
    }  # end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Effective sample size", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}
  }  # end of survey loop


##### EfN.fh  #### Removing the greatest 10% for each species
setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
if(fig.type == 'wmf') {win.metafile("./EfN_FH.wmf" , height = fig.ht.3, width = fig.wdth.3)}
par(mfrow=c(3,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
for (i in 1:nsp)  {
  par(las = 1)
  #if (FHwt[i] == 0)  {
  if(length(pred[pred==i])>0) {
    ytmp <- c(EfN.fh[[i]] , FHwt[i])
    ytmp <- ytmp[order(ytmp,decreasing=T)][(ceiling((nage[i]*nbins)/10)):length(ytmp)]
    xtmp <- 1:(nage[i]*nbins)
    plot(xtmp, EfN.fh[[i]], type="p",xlab="",ylab="",ylim=c(0,max(ytmp,na.rm=T)),axes=F,cex=p.size,lwd=p.width)
    lines(xtmp, rep(FHwt[i],length(xtmp)), lwd=l.width)  
    axis(side=1, at=axTicks(1), labels=FALSE, cex.axis=a.size,lwd=a.width)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
    box(lwd=a.width)
    mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
    }   else {
  # leave empty slot
    plot(yrs[i,],rep(1,length(yrs[i,])),type="l",col=par("bg"),axes=F,xlab="",ylab="") }
  # if(i >= 7)  {axis(side=1, at=axTicks(1), labels=TRUE, col='black', cex.axis=a.size,lwd=a.width)}
  }  # end of species loop
  par(las = 0)
  mtext("PredAge,Yr Combination",side=1,line=1,cex=label.size,outer=T)
  mtext("Effective sample size", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}



##### EfN.fh  #### Removing the greatest 10% for each species
setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
if(fig.type == 'wmf') {win.metafile("./EfN_FH_ModY.wmf" , height = fig.ht.3, width = fig.wdth.3)}
par(mfrow=c(3,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
for (i in 1:nsp)  {
  par(las = 1)
  #if (FHwt[i] == 0)  {
  if(length(pred[pred==i])>0) {
    ytmp <- c(EfN.fh[[i]] , FHwt[i])
    ytmp <- ytmp[order(ytmp,decreasing=T)][(ceiling((nage[i]*nbins)/10)):length(ytmp)]
    
    xtmp <- 1:(nage[i]*nbins)
    plot(xtmp, EfN.fh[[i]], type="p",xlab="",ylab="",ylim=c(0,min(max(ytmp,na.rm=t),200)),axes=F,cex=p.size,lwd=p.width)
    lines(xtmp, rep(FHwt[i],length(xtmp)), lwd=l.width)  
    axis(side=1, at=axTicks(1), labels=FALSE, cex.axis=a.size,lwd=a.width)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
    box(lwd=a.width)
    mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
    }   else {
  # leave empty slot
    plot(yrs[i,],rep(1,length(yrs[i,])),type="l",col=par("bg"),axes=F,xlab="",ylab="") }
  # if(i >= 7)  {axis(side=1, at=axTicks(1), labels=TRUE, col='black', cex.axis=a.size,lwd=a.width)}
  }  # end of species loop
  par(las = 0)
  mtext("PredAge,Yr Combination",side=1,line=1,cex=label.size,outer=T)
  mtext("Effective sample size", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}



###########################################################
##### Calculating standard deviations and number of observations in each dataset

stdevs <- matrix(NA,nrow=nsp,ncol=7)
  colnames(stdevs) <- c("totc","cprop","totfic1","totfic2","sprop1","sprop2","fh")
  rownames(stdevs) <- sp.names
nobs <- matrix(NA,nrow=nsp,ncol=7)
  colnames(nobs) <- c("totc","cprop","totfic1","totfic2","sprop1","sprop2","fh")
  rownames(nobs) <- sp.names

for (i in 1:nsp)
  {
  if(TCwt[i] > 0)  {
    stdevs[i,"totc"] <- sd(as.vector(sp9.rep$resid.totc[i,]))
    nobs[i,"totc"] <- length(na.omit(as.vector(sp9.rep$resid.totc[i,])))
    }
  if(CPwt[i] > 0)  {
    stdevs[i,"cprop"] <- sd(na.omit( as.vector(multi.resid.cprop[[i]]) ))
    nobs[i,"cprop"] <- length(na.omit( as.vector(multi.resid.cprop[[i]]) ))
    }
  for(j in 1:sp9.rep$nFIC)  {
    if(TSwt[i,j] > 0)  {
      stdevs[i,paste("totfic",j,sep="")] <- sd(as.vector( resid.totfic[[i]][j,] ))
      nobs[i,paste("totfic",j,sep="")]   <- length(na.omit(as.vector( resid.totfic[[i]][j,] )))
      } # end of TSwt if statement
    if(SPwt[i,j] > 0)  {
      stdevs[i,paste("sprop",j,sep="")] <- sd(na.omit( as.vector(multi.resid.sprop[[i]][[j]]) ))
      nobs[i,paste("sprop",j,sep="")]   <- length(na.omit( as.vector(multi.resid.sprop[[i]][[j]]) ))
      } # end of SPwt if statement
    } # end of survey loop    
  if(FHwt[i] > 0)  {
    stdevs[i,"fh"] <- sd(na.omit( as.vector(multi.resid.fh[[i]]) ))
    nobs[i,"fh"] <- length(na.omit( as.vector(multi.resid.fh[[i]]) ))
    }
  }  #end of species loop
  
  
max.M2 <- unlist(lapply(M2,max))
min.M2 <- unlist(lapply(M2,min))
M2.summary <- cbind(min.M2,max.M2)


write.table(round(stdevs,4),"Residuals Stdevs.txt",sep="\t")
write.table(nobs,"Number of observations.txt",sep="\t")
write.table(max.M2,"Max_Predicted_M2.txt",sep="\t")
write.table(M2.summary,"Max_Min_PredictedM2.txt",sep="\t")

stdevs
sp9.rep$ofv
sp9.rep$mgc

