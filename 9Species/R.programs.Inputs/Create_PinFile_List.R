###########################################
#Creates list of pin file inputs
###########################################


# Check ssp.Age1 and N units
sp.tmp <- 1
rect1 <- exp(ssp.Age1[[sp.tmp]])
  rect2 <- ssp.N[[sp.tmp]][2:nrow(ssp.N[[1]]),"1"]
  max(abs(rect1-rect2))
log.rect1 <- ssp.Age1[[sp.tmp]]
  log.rect2 <- log(ssp.N[[sp.tmp]][2:nrow(ssp.N[[1]]),"1"])
  max(abs(log.rect1-log.rect2))

# Rect: ssp.Age1 is already on a log scale
ssp.aAge1 <- lapply(ssp.Age1,mean)
ssp.dAge1 <- lapply(ssp.Age1, function(x) {x - mean(x)})

# ssp.Ft is *not* in on a log scale
ssp.Ft.nonzero <- lapply(ssp.Ft, function(x){x[x!=0]})
ssp.log.Ft <- lapply(ssp.Ft.nonzero,log)
ssp.aFt <- lapply(ssp.log.Ft,mean)
ssp.dFt <- lapply(ssp.log.Ft, function(x) {x - mean(x)})

# Survey and fishery selectivity parameters
ssp.survey.sel <- list.bysp
ssp.fishery.sel <- list.bysp
for (i in 1:nsp)  {
  sp <- sp.names[i]
  ssp.survey.sel[[sp]] <- ssp.ficsel[[sp]][1:(ssp.ficFR[[sp]]-1)]
  ssp.fishery.sel[[sp]] <- ssp.sel[[sp]][ssp.agePR[[sp]]:(ssp.ageFR[[sp]]-1)]
  }

# Yr1 is not on a log-scale
ssp.log.Yr1 <- lapply(ssp.Yr1,log)

Sp9.pin <- c(
  list(
    Run = Run,
    iRho = log(iRho)
  ),
  avgAge1 = ssp.aAge1,
  avgFt = ssp.aFt,
  devAge1 = ssp.dAge1,
  devFt = ssp.dFt,
  SurveyS = ssp.survey.sel,
  FisheryS = ssp.fishery.sel,
  iYr1 = ssp.log.Yr1
)


