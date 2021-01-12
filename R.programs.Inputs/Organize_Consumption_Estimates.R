################################################################################
# Organize consumption estimates for input into msp model
# Consumption estimates were initially estimated in SAS and modified in Import_Modify_Consumption_Rates.R
################################################################################

# CB.avg

CB.nbins
FH.nyr = FH.lyr-FH.fyr+1

yrbin.vec <- rep(1:CB.nbins, each=FH.nyr/CB.nbins)

cbtmp <- CB.avg[['Her']]

# Function repeats CB estimates for each year bin X times
cb.by.year <- function(cbtmp){
  cb.u <- cbtmp[,c('PdAge','YrBin','CB2')]
  cb.v <- reshape(cb.u, idvar="YrBin",timevar="PdAge", direction='wide', v.names="CB2")
  cb.w <- split(cb.v,cb.v$YrBin) 
  cb.x <- lapply(cb.w, function(x){x[rep(1,(FH.nyr/CB.nbins)),]} )
  cb.y <- do.call(rbind,cb.x)
    rownames(cb.y) <- as.character(FH.fyr:FH.lyr)
    cb.y$YrBin <- NULL
    colnames(cb.y) <- do.call(rbind,strsplit(colnames(cb.y),"\\."))[,2]
  cb.y  
  }
  
CB.list <- lapply(CB.avg, cb.by.year)
CB.list <- lapply(CB.list,as.matrix)  
