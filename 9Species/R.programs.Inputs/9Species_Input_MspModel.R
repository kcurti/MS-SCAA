rm(list=ls())

Run <- c("Re-create Run64")

# Most recent ssp and sp3 runs, from which I will base the pin file
cod.run <- c("Run119")
sh.run <- c("Run157")
her.run <- c("Run225")
mack.run <- c("Run18") 
goose.run <- c("Run8")
wh.run <- c("Run18") 
pol.run <- c("Run3a") 
sdog.run <- c("Run61")
wsk.run <- c("Run27") 
sp3.run <- c("Run59")

# Paths to folders
dissertation.data <- c("C:/Documents and Settings/Kiersten L Curti/My Documents/Dissertation/Data/")
my.programs <- c("C:/My_Programs/")
sp9.dir <- paste(my.programs,"ADMB/9Species",sep="")

# Import R functions
path="C:/My_Programs/R/Functions/Kdat_write.R"
Kdat_write<-dget(path);
path="C:/My_Programs/R/Functions/Kpin_write.R"
Kpin_write<-dget(path);
path=paste(my.programs,"R/Functions/reptoRlist.R",sep="")
reptoRlist<-dget(path);

# Input previous report files
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Input_ReportFiles.R")

# Identify species information
nsp <- 9 
sp.names <- c("SDog","WSk","Goose","Cod","Mack","Pol","WH","SH","Her")
    # Sp order by descending number of age-classes
sp.order <- 1:nsp
names(sp.order) <- sp.names
mod.sp.names <- sp.names
  names(mod.sp.names) <- sp.names
  mod.sp.names['SH'] <- 'SHake'
  mod.sp.names['WH'] <- 'WHake'
# ID'g which species had length structured ssp runs (bc inputs differ)
length.struct <- rep(0,length(sp.names))
  names(length.struct) <- sp.names
length.struct[c('SDog','WSk')] <- 1
length.struct.sp <- names(length.struct[length.struct==1])

# Identify year information
fyr<-rep(1978,nsp)
lyr<-rep(2007,nsp)
  names(fyr) <- sp.names
  names(lyr) <- sp.names

# Food habits data
FH.fyr<-1978
FH.lyr<-2007
binsize <- 5
# Ecosystem biomass
eco.b = 15;                        ##Original units = million metric tons
eco.b = eco.b*1000;           ##Units = 10^6 kg;  Conversion:  million metric tons * 1e6(=million) * 1000(kg=metric ton) / 1e6(=million)

# Constants
eof <- 54321
o.constant <- 1e-3
p.constant <- 1e-30

# Phases: 
aAge1.ph <- rep(1,nsp)
  names(aAge1.ph) <- sp.names
  aAge1.ph[length.struct.sp] <- -1
aFt.ph <- aAge1.ph
dAge1.ph <- aAge1.ph
  dAge1.ph[dAge1.ph>0] <- 1 # 3,1 
dFt.ph <- dAge1.ph
Yr1.ph <- aAge1.ph
  Yr1.ph[Yr1.ph>0] <- 1 # 2,1 
fic.ph <- aAge1.ph
  fic.ph[fic.ph>0] <- 1 # 4,1
fish.ph <- fic.ph


# Specify species interactions
int.names <- c( #'Pred-Prey'
      'Cod-Cod', # high stdev
      'Cod-SH',
      'Cod-Her',
      'Cod-Mack',   # high stdev   
      # 'Cod-WH',  #???    

      'SH-SH',
      'SH-Her',
      'SH-Mack', #???

      ## 'Goose-SDog',  #???
      'Goose-Cod',
      'Goose-SH', 
      'Goose-Her',
      'Goose-Mack', 
      ## ‘Goose-Goose’, #???

      # 'WH-WH', 
       'WH-SH',
       'WH-Her', 
      # 'WH-Mack', #??? # high stdev
 
      # 'Pol-Mack', #???
       'Pol-SH',
       'Pol-Her', 

      'SDog-SH', 
      'SDog-Her',
      'SDog-Mack', 
      'SDog-Cod', 
      # 'SDog-Goose', #???

       'WSk-SH', 
       'WSk-Her'
      # 'WSk-Mack'
)
print('Remember to modify species interactions if necessary')

# Organize data for each individual species into lists
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Organize_Individ_SpData.R")

# Organize food habits data
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Organize_FH_Data.R")

# Concatenate size preference parameters;  Size preference parameters were initially estimated in SAS
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Concatenate_SizePref_Parameters.R")

# Modify FH data to remove any observed %Wts representing species interactions that are not explicitly modeled
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Modify_Sp9_FH_ForSpInts.R")

# Import consumption estimates; Modify to accomodate low sample sizes; Consumption estimates were initially estimated in SAS
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Import_Modify_Consumption_Rates.R")

# Organize consumption estimates
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Organize_Consumption_Estimates.R")

# Rho
#  Initial parameter estimates
iRho <- rep(1,nint)
# Phase
Rho.ph <- rep(1,nint)# rep(1,nint) # 

# Obj fx weights for FH components
Owt <- 10
FHwt <- rep(10,length(sp.order))
  names(FHwt) <- names(sp.order)


ssp.CPwt[['Her']] <- 3

ssp.SPwt[['Mack']][1] <- 5
ssp.SPwt[['Her']][1] <- 10

# Create list for dat file
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Create_DatFile_List.R")
# Create list for pin file
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Create_PinFile_List.R")
# Create modified list for pin file (using the estimates that went *into* the ssp runs
source("C:\\My_Programs\\R\\9Species Model\\Input Files\\Create_PinFile_With.SSPInputs.R")

# Write dat and pin files
setwd(sp9.dir)
Kdat_write("9Species",Sp9.dat)
Kpin_write("9Species",Sp9.pin)
# Kpin_write("9Species",Sp9.SSp.pin)


########################### TO DO ############################
########################### TO DO ############################



