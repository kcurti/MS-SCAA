# Duh, cannot import original elasmobranch files because their parameters are not estimated in the 9species model

#rm(list=ls())
setwd('C:\\My_Programs\\ADMB\\9Species\\Orig_PinFiles_SSPRuns')

load('CodHerPin.RData')
load('SHSSp.RData')
load('MackerelPin.RData')
load('GoosefishPin.RData')
load('WhiteHakePin.RData')
load('PollockPin.RData')

WH.SSp.pin    <- WH.SSp.pin.A
Goose.SSp.pin <- GooseSSp.pin
Cod.SSp.pin    <- CodSSp.pin
Mack.SSp.pin <- MackSSp.pin
rm(WH.SSp.pin.A,GooseSSp.pin,CodSSp.pin,MackSSp.pin)

length(Goose.SSp.pin)
length(Cod.SSp.pin)
length(Mack.SSp.pin)
length(Pol.SSp.pin)
length(WH.SSp.pin)
length(SH.SSp.pin)
length(Her.SSp.pin)

aAge1 <- list.bysp
aFt      <- list.bysp
dAge1 <- list.bysp
dFt      <- list.bysp
FICsel <- list.bysp
agesel <- list.bysp
Yr1     <- list.bysp

# Loop through teleost species
for (i in 3:nsp)  {
  sp <- sp.names[i]
  ssp.pin <- get(paste(sp.names[i],"SSp.pin",sep="."))
  
  aAge1[[sp]]  <- ssp.pin[['aAge1']]
  aFt[[sp]]       <- ssp.pin[['aFt']]
  dAge1[[sp]]  <- ssp.pin[['dAge1']]
  dFt[[sp]]       <- ssp.pin[['dFt']]
  FICsel[[sp]]  <- ssp.pin[['iFICsel']]
  if(i == 8)  {
  agesel[[sp]]  <- ssp.pin[['iSel']]
  }  else {
  agesel[[sp]]  <- ssp.pin[['iageSel']]
  }
  Yr1[[sp]]      <- ssp.pin[['iYr1']]  
  }

# Loop through teleost species
for (i in 1:2)  {
  sp <- sp.names[i]

  aAge1[[sp]]  <- ssp.aAge1[[sp]]
  aFt[[sp]]       <- ssp.aFt[[sp]]
  dAge1[[sp]]  <- ssp.dAge1[[sp]]
  dFt[[sp]]       <- ssp.dFt[[sp]]
  FICsel[[sp]]  <- ssp.survey.sel[[sp]]
  agesel[[sp]]  <- ssp.fishery.sel[[sp]]
  Yr1[[sp]]      <- ssp.log.Yr1[[sp]]
  }

Sp9.SSp.pin <- c(
  list(
    Run = Run,
    iRho = log(iRho)
  ),
  avgAge1 = aAge1,
  avgFt      = aFt,
  devAge1 = dAge1,
  devFt      = dFt,
  SurveyS  = FICsel,
  FisheryS  = agesel,
  iYr1        = Yr1
)