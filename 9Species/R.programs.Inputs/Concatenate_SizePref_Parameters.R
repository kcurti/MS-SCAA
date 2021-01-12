################################################################################
# Concatenate size preference parameters (estimated in SAS) for input into msp model

# Chapter 2 Modification: Estimated one set of size prefernce parameters for each ***predator species*** 
#             		                     instead of species interaction due to sample size limitations
#             		                     To avoid code modifications, will create size preference parameters for each
#             		                     species interaction, but values will be constant within each predator species
 
################################################################################


#Recall:   int.names   pred.names    prey.names

sizepref.dir <- c("C:\\Users\\Kiersten L Curti\\Documents\\Dissertation\\Data\\NEFSC Food Habits Data\\Indep_of_TimeStep\\PreyLengths-9Species\\")

# Obtain species for which I need size preference parameters (i.e. ID all predator species)
sizepref.names <- unique(pred.names)
  names(sizepref.names) <- sizepref.names
# Put sizepref.names in correct species order
tmp <- sizepref.names[names(sp.order)]
  sizepref.names <-  tmp[is.na(tmp)==FALSE]
  rm(tmp)
# Species directory names
sizepref.dir.names <- sizepref.names
  sizepref.dir.names['SH'] <- 'SHake'
  sizepref.dir.names['WH'] <- 'WHake'

# Eta and sigma are not in log space
Eta.pdsp  <- list.bysp[sizepref.names]
Sig1.pdsp <- list.bysp[sizepref.names]
Sig2.pdsp <- list.bysp[sizepref.names]

# Fill in lists with individual species estimates
for (i in 1:length(Eta.pdsp))  {
  sp <- sizepref.names[i]
  sp.dir <- paste(sizepref.dir, sizepref.dir.names[sp], "_AllPy",sep="")
  setwd(sp.dir)  
  assign(paste(sp,'sizepref.rep',sep="."), reptoRlist(paste(sizepref.dir.names[sp],"SizePref.rep",sep="")) )

  Eta.pdsp[sp] <- get(paste(sp,'sizepref.rep',sep="."))$eta
  Sig1.pdsp[sp] <- get(paste(sp,'sizepref.rep',sep="."))$sig1
  Sig2.pdsp[sp] <- get(paste(sp,'sizepref.rep',sep="."))$sig2
  }

# Repeat predator-specific size-preference parameters for each species interaction

# Matrices (predator X prey)
Eta <- matrix(0,nrow=nsp,ncol=nsp)
  rownames(Eta) <- sp.names
  colnames(Eta) <- sp.names
Sig1 <- Eta
Sig2 <- Eta
# Vectors
Eta.vec <- rep(NA,length(pred.names))
  names(Eta.vec) <- pred.names
Sig1.vec <- Eta.vec
Sig2.vec <- Eta.vec
# Fill matrices
for (i in 1:nint)  
  {
  pd <- pred.names[i]
  py <- prey.names[i]  
  Eta[pd,py]  <- Eta.pdsp[[pd]]
  Sig1[pd,py] <- Sig1.pdsp[[pd]]
  Sig2[pd,py] <- Sig2.pdsp[[pd]]   
  }
# Fill vectors
for (i in 1:length(Eta.vec))  
  {
  pd <- names(Eta.vec)[i]
  Eta.vec[i]  <- Eta.pdsp[[pd]]
  Sig1.vec[i] <- Sig1.pdsp[[pd]]
  Sig2.vec[i] <- Sig2.pdsp[[pd]]
  }


# Chapter 1 estimates:
# Interactions:   Cod-Cod   Cod-SH   Cod-Her   SH-SH   SH-Her
# Ch1.iEta <- 	exp(c(1.425271, 1.575552, 1.385288, 1.372632, 0.8156918))	
# Ch1.iSig <- 	exp(c(0.8150397, 0.6288599, 0.3595316, 1.091466, 0.08870662))
# Ch1.iRho <- exp(c(0.617751227, 1.549295379, 3.25448427, 5.395996944, 3.222489669))

# Ch1.aEta <- iEta
# Ch1.aSig <- iSig
# Ch1.sEta <- rep(0.1,length(iEta))
# Ch1.sSig <- rep(0.1,length(iSig))



