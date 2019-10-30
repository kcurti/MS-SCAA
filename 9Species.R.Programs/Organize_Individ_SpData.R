###########################################
#Arranges SSP results into lists for input into msp model
###########################################


seasons <- c('Spring','Fall')

# List templates
list.bysp <- vector('list',nsp)
  names(list.bysp) <- sp.names
list.bysp.season <- lapply(list.bysp,function(x){
                                     x<-vector('list',length(seasons))
                                     names(x) <- seasons
                                     x  })

##### Create lists for each ssp data inputs ##### 

ssp.yrs <- list.bysp

# Numbers: Input into list to determine if the same across species
ssp.nFIC <- list.bysp
ssp.M1nseg <- list.bysp

# Data vectors
ssp.nage <- list.bysp

ssp.FICnseg <- list.bysp
ssp.FICmon <- list.bysp
ssp.FICyr <- list.bysp
ssp.M1yr <- list.bysp

ssp.FICfage <- list.bysp
ssp.FIClage <- list.bysp
ssp.FICs.lage <- list.bysp

ssp.agePR <- list.bysp
ssp.ageFR <- list.bysp
ssp.ficFR <- list.bysp

# Dataset weights
ssp.TCwt <- list.bysp
ssp.CPwt <- list.bysp
ssp.Bwt <- list.bysp
ssp.Ywt <- list.bysp
ssp.Rwt <- list.bysp
ssp.Bthres <- list.bysp
ssp.Rthres <- list.bysp
ssp.TSwt  <- list.bysp
ssp.SPwt  <- list.bysp

# Objective function info
ssp.ofv <- list.bysp
ssp.ofvsp <- list.bysp
ssp.ofv.ideal  <- list.bysp
ssp.TCres <- list.bysp
ssp.TSres <- list.bysp
ssp.CP.rev <- list.bysp
ssp.SP.rev <- list.bysp

# Time series
ssp.CAA <- list.bysp
    ssp.pred.cprop <- list.bysp
ssp.N <- list.bysp
ssp.Wt <- list.bysp
ssp.TotC <- list.bysp
ssp.TotFIC <- list.bysp
ssp.M1seg <- list.bysp

ssp.FIC <- list.bysp.season
     ssp.pred.sprop <- list.bysp.season

# Initial parameter estimates
ssp.Age1 <- list.bysp
ssp.Ft <- list.bysp
ssp.sel <- list.bysp
ssp.ficsel <- list.bysp
ssp.Yr1 <- list.bysp



##### Fill lists for each ssp data inputs ##### 

for (i in 1:nsp)  {
  sp <- sp.names[i]
  ssp.rep <- get(paste(sp,"rep",sep="."))

  ssp.yrs[[sp]] <- ssp.rep[['yrs']]

  ssp.nFIC[[sp]] <- ssp.rep[['nFIC']]
  ssp.M1nseg[[sp]] <- ssp.rep[['M1nseg']]

  ssp.nage[[sp]] <- ssp.rep[['nage']]
  ssp.FICnseg[[sp]] <- ssp.rep[['FICnseg']]
  ssp.FICmon[[sp]] <- ssp.rep[['FICmonth.1']]
  ssp.FICyr[[sp]] <- ssp.rep[['FIC.yr.1']]
  ssp.M1yr[[sp]] <- ssp.rep[['M1yr']]

  ssp.FICfage[[sp]] <- ssp.rep[['FICfage']]
  ssp.FIClage[[sp]] <- ssp.rep[['FIClage']]

  ssp.agePR[[sp]] <- ssp.rep[['agePR']]
  ssp.ageFR[[sp]] <- ssp.rep[['ageFR']]
  ssp.ficFR[[sp]] <- ssp.rep[['ficFR']]
  ssp.FICs.lage[[sp]] <- ssp.rep[['FICs.lage']]

  ssp.TCwt[[sp]] <- ssp.rep[['TCwt']]
  ssp.CPwt[[sp]] <- ssp.rep[['CPwt']]
  ssp.Bwt[[sp]] <- ssp.rep[['Bwt']]
  ssp.Ywt[[sp]] <- ssp.rep[['Ywt']]
  ssp.Rwt[[sp]] <- ssp.rep[['Rwt']]
  ssp.Bthres[[sp]] <- ssp.rep[['Bthres']]
  ssp.Rthres[[sp]] <- ssp.rep[['Rthres']]
  ssp.TSwt[[sp]] <- ssp.rep[['TSwt']]
  ssp.SPwt[[sp]] <- ssp.rep[['SPwt']]

  ssp.ofv[[sp]] <- ssp.rep[['ofv']]
  ssp.ofvsp[[sp]] <- ssp.rep[['ofvsp']]
  ssp.ofv.ideal[[sp]] <- ssp.rep[['ofv_ideal']]
  ssp.TCres[[sp]] <- ssp.rep[['TCres']]
  ssp.TSres[[sp]] <- ssp.rep[['TSresA']]
  ssp.CP.rev[[sp]] <- ssp.rep[['CPmulti']] - ssp.rep[['CPideal']]
  ssp.SP.rev[[sp]] <- ssp.rep[['SPmulti']] - ssp.rep[['SPideal']]

  ssp.N[[sp]] <- ssp.rep[['N.1']]
    rownames(ssp.N[[sp]]) <- ssp.rep[['yrs']]
    colnames(ssp.N[[sp]]) <- 1:ssp.rep[['nage']]
  ssp.Wt[[sp]] <- ssp.rep[['Wt.1']]
    rownames(ssp.Wt[[sp]]) <- ssp.rep[['yrs']]
    colnames(ssp.Wt[[sp]]) <- 1:ssp.rep[['nage']]
  ssp.M1seg[[sp]] <- ssp.rep[['M1.1']]
    if(is.null(dim(ssp.M1seg[[1]])))  {names(ssp.M1seg[[sp]]) <- 1:ssp.rep[['nage']]} else {
                                                             rownames(ssp.M1seg[[sp]]) <- 1:ssp.rep[['nage']]}

  # Inputted time series = the **observed data**

  if(length.struct[sp] == 1)  {
    # Length-structured species only: Use predicted TotC,TotFIC,CAA,FIC because observed are not age-structured
    ssp.TotC[[sp]] <- ssp.rep[['pred.totc']]
    ssp.TotFIC[[sp]] <- ssp.rep[['pred.totFIC.1']]
    ssp.CAA[[sp]] <- ssp.rep[['pred.CAA.1']]
    for (j in 1:length(seasons))  {
      ssp.FIC[[sp]][[j]] <- ssp.rep[[ paste("pred.FIC",j,"sp.1",sep=".") ]]
      }  # end of season loop
    } else
    {
    # Age-structured species only: Use observed data
    ssp.TotC[[sp]] <- ssp.rep[['obs.totc']]
    ssp.TotFIC[[sp]] <- ssp.rep[['obs.totFIC.1']]
    ssp.CAA[[sp]] <- ssp.rep[['obs.CAA.1']]
    for (j in 1:length(seasons))  {
      ssp.FIC[[sp]][[j]] <- ssp.rep[[ paste("obs.FIC",j,"sp.1",sep=".") ]]
      }  # end of season loop      
    } # end of age-structured If Statement
   
  names(ssp.TotC[[sp]]) <- ssp.rep[['yrs']]
  colnames(ssp.TotFIC[[sp]]) <- ssp.rep[['yrs']]
  rownames(ssp.CAA[[sp]]) <- ssp.rep[['yrs']]
  colnames(ssp.CAA[[sp]]) <- 1:ssp.rep[['nage']]
  for (j in 1:length(seasons))  {
    rownames(ssp.FIC[[sp]][[j]]) <- ssp.rep[['yrs']]
    colnames(ssp.FIC[[sp]][[j]]) <- 1:ssp.rep[['nage']]  } # end of season loop


  ssp.Age1[[sp]] <- ssp.rep[['Age1']]
    names(ssp.Age1[[sp]]) <- ssp.rep[['yrs']][2:length(ssp.rep[['yrs']])]
  ssp.Ft[[sp]] <- ssp.rep[['Ft']]
    names(ssp.Ft[[sp]]) <- ssp.rep[['yrs']]

  ssp.sel[[sp]] <- ssp.rep[['sel.1']]
    names(ssp.sel[[sp]]) <- 1:ssp.rep[['nage']] 
  ssp.ficsel[[sp]] <- ssp.rep[['FICsel.1']]
    names(ssp.ficsel[[sp]]) <- 1:ssp.rep[['nage']] 
  
  ssp.Yr1[[sp]] <- ssp.N[[sp]][as.character(fyr[sp]),]
    names(ssp.Yr1[[sp]]) <- 1:ssp.rep[['nage']] 
 
  }  # end of species loop

## Modify Bthres for SH and Her from SSP runs bc have since updated value
ssp.Bthres[c('Cod','SH','Her')] <- 1e-7

