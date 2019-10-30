###########################################################
# Plot predicted indices

# Total biomass

###########################################################

library(RColorBrewer)
Greys <- rev(brewer.pal(7,'Greys'))  # rev reverses the elements in a vector


###########################################################
##### M2
M2.sp <- names(max.M2)[max.M2!=0]
M2.nage <- rep(6,length(M2.sp)) #number of age classes in the M2 figure
  names(M2.nage) <- M2.sp

setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.2, width = fig.wdth.2)}
if(fig.type == 'wmf') {win.metafile("./M2_AllPrey.wmf" , height = fig.ht.2, width = fig.wdth.2)}
par(mfrow=c(2,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
for (sp in 1:length(M2.sp))  {
  py <- M2.sp[sp]
  y.range = c(0,round(max.M2[[py]],3))
  par(las = 1) # horizontal text
  plot( yrs[py,],M2[[py]][,1], type="l",col=Greys[1],lty=lty.key[1],  xlab="",ylab="",xlim=yr.lim,ylim=y.range,lwd=l.width,axes=F)
  for (a in 2: M2.nage[py])  {
    lines(yrs[py,],M2[[py]][,a],col=Greys[a],type="l",lwd=l.width, lty=lty.key[a])
    }  # end of age loop
  axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
  if(sp >= 4)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
  mtext(fig.names[py], side=3, cex=fig.name.cex, line=0.3, font=2)
  box(lwd=a.width)
  } # end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Predation mortality", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}



###########################################################
##### Proportion of Z due to M2

setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.2, width = fig.wdth.2)}
if(fig.type == 'wmf') {win.metafile("./PropM2_AllPrey.wmf" , height = fig.ht.2, width = fig.wdth.2)}
max.prop.M2 <- lapply(prop.M2,max)
par(mfrow=c(2,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis

for (sp in 1:length(M2.sp))  {
  py <- M2.sp[sp]
  y.range = c(0,round(max.prop.M2[[py]],3))
  par(las = 1) # horizontal text
  plot( yrs[py,],prop.M2[[py]][,1], type="l",col=Greys[1],lty=lty.key[1],  xlab="",ylab="",xlim=yr.lim,ylim=y.range,lwd=l.width,axes=F)
  for (a in 2: M2.nage[py])  {
    lines(yrs[py,],prop.M2[[py]][,a],col=Greys[a],type="l",lwd=l.width, lty=lty.key[a])
    }  # end of age loop
  axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
  if(sp >= 4)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
  mtext(fig.names[py], side=3, cex=fig.name.cex, line=0.3, font=2)
  box(lwd=a.width)
  } # end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Proportion of Z", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}



###########################################################
##### F

# Recall that Ft on the natural scale
setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
if(fig.type == 'wmf') {win.metafile("./F_AllSp.wmf" , height = fig.ht.3, width = fig.wdth.3)}
par(mfrow=c(3,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
for (i in 1:nsp)  {
  par(las = 1) # horizontal text
  ytmp<-c(Ft[[i]] , ssp.Ft[[i]])
  plot(yrs[i,],Ft[[i]], type="l",xlab="",ylab="", xlim=yr.lim,ylim=c(0,max(ytmp)),axes=F, col=msp.col,lty=msp.lty, cex=p.size,lwd=l.width)
  lines(yrs[i,],ssp.Ft[[i]], col=ssp.col, lty=ssp.lty, cex=p.size,lwd=l.width)  
  axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
  if(i >= 7)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
  mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
  box(lwd=a.width)
  } # end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Fishing mortality", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}



###########################################################
##### Recruitment

# Recall that Age1 on a log scale
setwd(fig.dir)
if(fig.type == 'gui')    {dev.new(height = fig.ht.3, width = fig.wdth.3)}
if(fig.type == 'wmf') {win.metafile("./Rect_AllSp.wmf" , height = fig.ht.3, width = fig.wdth.3)}
par(mfrow=c(3,3))
par(mar=c(2, 2.6, 0.3, 1) +0.1);  par(oma=c(2.5,2.5,2.3,0)) # Horizontal y-axis
for (i in 1:nsp)  {
  par(las = 1) # horizontal text
  ytmp <- exp(unlist( c(ssp.Age1[[i]],Age1[[i]]) ))
  plot(names(Age1[[i]]),exp(Age1[[i]]), type="l",xlab="",ylab="", xlim=yr.lim,ylim=c(0,max(ytmp)),axes=F, col=msp.col,lty=msp.lty, cex=p.size,lwd=l.width)
  lines(names(ssp.Age1[[i]]), exp(ssp.Age1[[i]]), col=ssp.col, lty=ssp.lty, cex=p.size,lwd=l.width)  
  axis(side=1, at=yr.ticks, labels=FALSE, cex.axis=a.size,lwd=a.width)
  if(i >= 7)  {axis(side=1, at=yr.ticks, labels=TRUE, cex.axis=a.size,lwd=a.width)}
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size,lwd=a.width)
  mtext(fig.names[i], side=3, cex=fig.name.cex, line=0.3, font=2)
  box(lwd=a.width)
  } # end of species loop
  par(las = 0)
  mtext("Year",side=1,line=1,cex=label.size,outer=T)
  mtext("Recruitment (millions of fish)", side=2, line=1, cex=label.size,outer=T) 
  if (fig.type == 'wmf') {dev.off()}





