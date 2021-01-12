###########################################
#Creates list of dat file inputs
###########################################

# For each of the following variables, if the # unique values is > 1, I need to manually modify
nFIC.num <- unique(range(unlist(ssp.nFIC)))
    if(length(nFIC.num)>1) {print("STOP!!!!!!!!!!!!!!!!!!!");print("STOP!!!!!!!!!!!!!!!!!!!");print("STOP!!!!!!!!!!!!!!!!!!!")}
M1nseg.num <- unique(range(unlist(ssp.M1nseg))) # if Mnseg is not 1 number, dat file will not be correct
    if(length(M1nseg.num)>1) {print("STOP!!!!!!!!!!!!!!!!!!!");print("STOP!!!!!!!!!!!!!!!!!!!");print("STOP!!!!!!!!!!!!!!!!!!!")}
M1yr.num <- unique(range(unlist(ssp.M1yr))) # if Mnseg is not 1 number, dat file will not be correct
    if(length(M1yr.num)>1) {print("STOP!!!!!!!!!!!!!!!!!!!");print("STOP!!!!!!!!!!!!!!!!!!!");print("STOP!!!!!!!!!!!!!!!!!!!")}

# Adjusting prey M1's
Sp9.M1seg <- ssp.M1seg
  Sp9.M1seg[['Mack']][] <- 0.1
  
# Modify sig = 0 to sig = -99  
Sig1.out <- Sig1
  Sig1.out[Sig1.out==0] <- -99
Sig2.out <- Sig2
  Sig2.out[Sig2.out==0] <- -99

Sp9.dat <- c(
  list (
    Run = Run,
    Simulation = 1,
    Trophic = 1,
    nsp = nsp,
    nFIC = nFIC.num,
    o = o.constant,
    p = p.constant,
    Nint = nint,
    Binsize = binsize,
    EcoB.mil.kg = eco.b,
    Owt = Owt,
    Fyr=fyr, Lyr=lyr,
    FHfyr=FH.fyr,FHlyr=FH.lyr,
    Nage = unlist(ssp.nage),
    Mnseg = M1nseg.num, 
    Nseg = unlist(ssp.FICnseg),
    agePR = unlist(ssp.agePR),
    ageFR = unlist(ssp.ageFR),
    ficFR = unlist(ssp.ficFR),
    FICs_lage = unlist(ssp.FICs.lage),

    aAge1ph = aAge1.ph,
    aFtph = aFt.ph,
    dAge1ph = dAge1.ph,
    dFtph = dFt.ph,
    ficph = fic.ph,
    fishph = fish.ph,
    Yr1ph = Yr1.ph,  

    Pred = pred,
    Prey = prey,
    Rhoph = Rho.ph,
    M1yr = M1yr.num,
    TCwt = unlist(ssp.TCwt),
    CPwt = unlist(ssp.CPwt),
    Bwt = unlist(ssp.Bwt),
    Ywt = unlist(ssp.Ywt),
    Rwt = unlist(ssp.Rwt),
    FHwt = FHwt,
    Bthres = unlist(ssp.Bthres),
    Rthres = unlist(ssp.Rthres),
    TSwt = do.call(rbind,ssp.TSwt),
    SPwt = do.call(rbind,ssp.SPwt),
    iM2 = rep(0,sum(unlist(ssp.nage))),
    Eta = Eta,
    Sigma1 = Sig1.out,
    Sigma2 = Sig2.out,
    fic_fage = do.call(rbind,ssp.FICfage),
    fic_lage = do.call(rbind,ssp.FIClage),
    FICmonth = do.call(rbind,ssp.FICmon),
    FICyr = unlist(ssp.FICyr)
  ),

  TotC.tmt = ssp.TotC,
  WeightAtAge.kg=ssp.Wt,
  CatchAtAge.mil = ssp.CAA,
  TotFIC = ssp.TotFIC, 
  M1 = Sp9.M1seg,
  CBratio = CB.list,

   FIC.SDog.1  = lapply(ssp.FIC[[1]],as.matrix),
   FIC.WSk.2   = lapply(ssp.FIC[[2]],as.matrix), 
   FIC.Goose.3 = lapply(ssp.FIC[[3]],as.matrix), 
   FIC.Cod.4    = lapply(ssp.FIC[[4]],as.matrix),
   FIC.Mack.5 = lapply(ssp.FIC[[5]],as.matrix),  
   FIC.Pol.6     = lapply(ssp.FIC[[6]],as.matrix), 
   FIC.WH.7   = lapply(ssp.FIC[[7]],as.matrix),    
   FIC.SH.8    = lapply(ssp.FIC[[8]],as.matrix), 
   FIC.Her.9   = lapply(ssp.FIC[[9]],as.matrix),  
  FH = Sp9.ModFH.list,
  Eof = list(eof)
 )



