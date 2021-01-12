###################################
#Organizing MSP outputs into lists
###################################

 # 9Species report file must be named sp9.rep

TCwt <- sp9.rep$TCwt
CPwt <- sp9.rep$CPwt
TSwt <- sp9.rep$TSwt
  rownames(TSwt) <- sp.names
  colnames(TSwt) <- seasons
SPwt <- sp9.rep$SPwt
  rownames(SPwt) <- sp.names
  colnames(SPwt) <- seasons
FHwt <- sp9.rep$FHwt  

Ft   <- list.bysp
M1 <- list.bysp
M2 <- list.bysp
Z    <- list.bysp
prop.M2 <- list.bysp  #Proportion of total mortality due to M2

Wt <- list.bysp

Age1 <- list.bysp
N      <- list.bysp
TotN <- list.bysp
BinN <- list.bysp  #Average N in each year bin/interval
B      <- list.bysp
TotB <- list.bysp

fic.month <- list.bysp
fic.yr       <- list.bysp
survey.s  <- list.bysp
fishery.s  <- list.bysp
FIC.q      <- list.bysp

obs.totc    <- list.bysp
pred.totc   <- list.bysp
resid.totc   <- list.bysp
obs.cprop  <- list.bysp
pred.cprop <- list.bysp
multi.resid.cprop <- list.bysp
EfN.cprop <- list.bysp

obs.totfic   <- list.bysp
pred.totfic <- list.bysp
resid.totfic <- list.bysp
toteps        <- list.bysp
obs.sprop   <- list.bysp.season
pred.sprop  <- list.bysp.season
multi.resid.sprop <- list.bysp.season
EfN.sprop <- list.bysp.season

sum.Con <- list.bysp
obs.fh        <- list.bysp
pred.fh      <- list.bysp
EfN.fh      <- list.bysp
multi.resid.fh <- list.bysp
obs.fh.list   <- list.bysp
pred.fh.list  <- list.bysp
obs.fh.age  <- list.bysp
pred.fh.age <- list.bysp
avg.pred.fh.age <- list.bysp
avg.fh.age.sub  <- list.bysp
avg.pred.fh.yr   <- list.bysp
avg.fh.yr.sub    <- list.bysp

fh.ymax <- 0.3


for (i in 1:nsp)
  {
  
  Ft[[i]] <- sp9.rep$Ft[i,]
    names(Ft[[i]]) <- sp9.rep$yrs[i,]
  M1[[i]] <- sp9.rep[[paste('M1.',i,sep='')]]
  M2[[i]] <- sp9.rep[[paste('M2.',i,sep='')]]
  Z[[i]] <- sp9.rep[[paste('Z.',i,sep='')]]
  prop.M2[[i]] <- M2[[i]]/Z[[i]]

  Wt[[i]] <- sp9.rep[[paste('Wt.',i,sep='')]]
    rownames(Wt[[i]]) <- sp9.rep$yrs[i,]
    colnames(Wt[[i]]) <- 1:sp9.rep$nage[i]

  Age1[[i]] <- sp9.rep$Age1[i,]
    names(Age1[[i]]) <- sp9.rep$yrs[i,2:ncol(sp9.rep$yrs)]
  N[[i]] <- sp9.rep[[paste('N.',i,sep='')]]
    rownames(N[[i]]) <- sp9.rep$yrs[i,]
    colnames(N[[i]]) <- 1:sp9.rep$nage[i]
  TotN[[i]] <- rowSums(N[[i]])
  tmp <- split(TotN[[i]],rep(1:nbins,each=yrbin))
  BinN[[i]] <- unlist(lapply(tmp,mean))
  B[[i]] <- N[[i]]*Wt[[i]]
  TotB[[i]] <- rowSums(B[[i]])

  fic.month[[i]]<-sp9.rep[[paste('FICmonth.',i,sep='')]]
  fic.yr[[i]] <- sp9.rep[[paste('FIC.yr.',i,sep='')]]
  survey.s[[i]] <-  sp9.rep[[paste('FICsel.',i,sep='')]]
    names(survey.s[[i]]) <- 1:sp9.rep$nage[i]
  fishery.s[[i]] <- sp9.rep[[paste('sel.',i,sep='')]]
    names(fishery.s[[i]]) <- 1:sp9.rep$nage[i]
  FIC.q[[i]] <- sp9.rep[[paste('FIC.q.',i,sep='')]]

  obs.totc[[i]] <- sp9.rep$obs.totc[i,]
  pred.totc[[i]] <- sp9.rep$pred.totc[i,]
  resid.totc[[i]] <- sp9.rep$resid.totc[i,]
    names(obs.totc[[i]]) <- sp9.rep$yrs[i,]
    names(pred.totc[[i]]) <- sp9.rep$yrs[i,]
    names(resid.totc[[i]]) <- sp9.rep$yrs[i,]
  obs.cprop[[i]] <- sp9.rep[[paste('obs.cprop.',i,sep='')]]
    # Set missing values to NA
    obs.cprop[[i]][apply(obs.cprop[[i]]==0,1,all),] <- NA
  pred.cprop[[i]] <- sp9.rep[[paste('pred.cprop.',i,sep='')]]
  EfN.cprop[[i]] <- Ef.N.calc(obs.cprop[[i]] , pred.cprop[[i]])  
  multi.resid.cprop[[i]] <- multi.res(obs.cprop[[i]] , pred.cprop[[i]] , obs.totc[[i]] , p)

  obs.totfic[[i]] <- sp9.rep[[paste('obs.totFIC.',i,sep='')]]
    colnames(obs.totfic[[i]]) <- sp9.rep$yrs[i,]
  pred.totfic[[i]] <- sp9.rep[[paste('pred.totFIC.',i,sep='')]]
    colnames(pred.totfic[[i]]) <- sp9.rep$yrs[i,]
  resid.totfic[[i]] <- sp9.rep[[paste('resid.totFIC.',i,sep='')]]
  toteps[[i]] <- sp9.rep[[paste('toteps.',i,sep='')]]
  for (k in 1:sp9.rep$nFIC)  {
    obs.sprop[[i]][[k]] <- sp9.rep[[paste('obs.sprop',k,'sp',i,sep='.')]]
      # Set missing values to NA
      obs.sprop[[i]][[k]][apply(obs.sprop[[i]][[k]]==0,1,all),] <- NA
    pred.sprop[[i]][[k]] <- sp9.rep[[paste('pred.sprop',k,'sp',i,sep='.')]]
    EfN.sprop[[i]][[k]] <- Ef.N.calc( obs.sprop[[i]][[k]] , pred.sprop[[i]][[k]] )    
    multi.resid.sprop[[i]][[k]] <- multi.res(obs.sprop[[i]][[k]] , pred.sprop[[i]][[k]] , obs.totfic[[i]][k,] , p)
    }


  # Total consumed
  sum.Con[[i]] <- sp9.rep[[paste('sumCon.',i,sep='')]]
    rownames(sum.Con[[i]]) <- sp9.rep$yrs[i,]
    colnames(sum.Con[[i]]) <- 1:sp9.rep$nage[i]


  #FH
  obs.fh[[i]]  <- sp9.rep[[paste('obs.fh.',i,sep='')]]
  #Marking missing input data with NA's
  obs.fh[[i]][apply(obs.fh[[i]]==9999.99,1,all),] <- NA  #NEW
  pred.fh[[i]] <- sp9.rep[[paste('pred.fh.',i,sep='')]]
  EfN.fh[[i]] <- Ef.N.calc( obs.fh[[i]] , pred.fh[[i]] )
  tot.fh.tmp <-  rowSums(obs.fh[[i]])
    tot.fh.tmp[is.na(tot.fh.tmp)==TRUE] <- 0
  multi.resid.fh[[i]] <- multi.res(obs.fh[[i]] , pred.fh[[i]] , tot.fh.tmp , p)



  nbins <- nrow(obs.fh[[1]])/nage[1]
  bin <- rep(1:nbins,each=nage[i])  
  
  obs.fh.list[[i]] <- split(obs.fh[[i]],bin)
  pred.fh.list[[i]] <- split(pred.fh[[i]],bin)
  for (b in 1:nbins)
    {
    obs.fh.list[[i]][[b]] <- matrix(obs.fh.list[[i]][[b]],nrow=nage[i],ncol=(nsp+1))
    pred.fh.list[[i]][[b]] <- matrix(pred.fh.list[[i]][[b]],nrow=nage[i],ncol=(nsp+1))
    }

  obs.fh.list[[i]] <- array(unlist(obs.fh.list[[i]]),c(nage[[i]],nsp+1,nbins))
  pred.fh.list[[i]] <- array(unlist(pred.fh.list[[i]]),c(nage[[i]],nsp+1,nbins))

  #Now reshaping the 3darray using aperm
  obs.fh.age[[i]] <- aperm(obs.fh.list[[i]],c(3,2,1))
  pred.fh.age[[i]] <- aperm(pred.fh.list[[i]],c(3,2,1))
    dimnames(obs.fh.age[[i]])[[1]] <- as.character(1:nbins) #paste("bin",as.character(1:nbins),sep="")
    dimnames(obs.fh.age[[i]])[[2]] <- FH.sp.names
    dimnames(pred.fh.age[[i]])[[1]] <- as.character(1:nbins) #paste("bin",as.character(1:nbins),sep="")
    dimnames(pred.fh.age[[i]])[[2]] <- FH.sp.names

  #Averaging over years to obtain an average predicted predator diet (by age)
  avg.pred.fh.age[[i]] <- t(apply(pred.fh.age[[i]],3,colMeans))
  rownames(avg.pred.fh.age[[i]]) <- as.character(1:nage[i])
  colnames(avg.pred.fh.age[[i]]) <- FH.sp.names
    #Only including up to a proportion of fh.ymax (instead of 1.0) because the majority of prey = other food
    avg.fh.age.sub[[i]] <- avg.pred.fh.age[[i]]
    avg.fh.age.sub[[i]][,"Other"] <- fh.ymax - rowSums(avg.pred.fh.age[[i]][,sp.names])

 #Average predator diet, averaging over ages (by year)
  avg.pred.fh.yr[[i]] <- t(apply(pred.fh.age[[i]],1,rowMeans))
  rownames(avg.pred.fh.yr[[i]]) <- as.character(1:nbins)
  colnames(avg.pred.fh.yr[[i]]) <- FH.sp.names
    #Only including up to a proportion of fh.ymax (instead of 1.0) because the majority of prey = other food
    avg.fh.yr.sub[[i]] <- avg.pred.fh.yr[[i]]
    avg.fh.yr.sub[[i]][,"Other"] <- fh.ymax - rowSums(avg.pred.fh.yr[[i]][,sp.names])


  }  #end of species i loop



