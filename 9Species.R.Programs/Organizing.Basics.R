# Organizing the basics (sp/ages/yrs/ints)

#Defining multinomial constant
p <- 1.e-30

#Organizing sp/ages/yrs/ints
nsp <- sp9.rep$nsp
nage <- sp9.rep$nage
  names(nage) <- sp.names
fage <- c(1,cumsum(nage)+1)
  names(fage) <- FH.sp.names
lage <- c(cumsum(nage),sum(nage)+1)
  names(lage) <- FH.sp.names

yrs <- sp9.rep$yrs
  rownames(yrs) <- sp.names
fh.fyr <- sp9.rep$FH.fyr
fh.lyr <- sp9.rep$FH.lyr
fh.yrs <- as.character(fh.fyr:fh.lyr)
# Identify year information
fyr <- yrs[,1]
lyr <- yrs[,ncol(yrs)]

pred <- sp9.rep$pred
prey <- sp9.rep$prey
nbins <- sp9.rep$nbins

ages <- rep(1,each=nage[1])
for (i in 2:nsp)  { ages <- c(ages,rep(i,each=nage[i])) }
py.ages <- c(ages,nsp+1)
nage.of <- c(nage,1)
  names(nage.of) <- FH.sp.names

#Tickmark details
#Axis = year
yr.axis <- range(yrs,na.rm=T)
yrbin <- 5
bins <- rep(1:nbins,each=yrbin)
yr.ticks <- seq(yr.axis[1],yr.axis[2]+1,by=yrbin)
yr.labels <- rep(NA,length(yr.ticks))
for (i in seq(1,length(yr.ticks),2))
  {yr.labels[i] <- yr.ticks[i]}
yr.lim <- c(yr.ticks[1],yr.ticks[length(yr.ticks)])
