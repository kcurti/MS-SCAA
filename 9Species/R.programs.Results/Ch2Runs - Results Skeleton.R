
rm (list=ls())
ls()

sp9.run <- "Run84"
fig.type <- 'wmf'

#my.programs <- c("C:/Users/kiersten.curti/My_Programs/")
my.programs <- c("C:/My_Programs/")

# Function to report files
path=paste(my.programs,"R/Functions/reptoRlist.R",sep="")
reptoRlist<-dget(path);
# Calculate effective sample size, Ef.N
path=paste(my.programs,"R/Functions/Calc_Effective_SampleSize.R",sep="")
  Ef.N.calc <-dget(path);
# Calculate multinomial residuals
path=paste(my.programs,"R/Functions/Calc.multinomial.residuals.R",sep="")
multi.res <- dget(path);

# Import 9Species report file
sp9.dir <- paste(my.programs,"ADMB/9Species/",sp9.run,sep="")
setwd(sp9.dir)
sp9.rep <- reptoRlist("9Species.rep")
sp9.rep$ofv
sp9.rep$mgc
# Import ssp and 3Species report files
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
source(paste(my.programs,"R/9Species Model/Input Files/Input_ReportFiles.R",sep=""))

# Set directory for figure output
fig.dir <- sp9.dir

sp.names <- c("SDog","WSk","Goose","Cod","Mack","Pol","WH","SH","Her")
FH.sp.names <- c(sp.names,'Other')
fig.names <- c("Spiny dogfish", "Winter skate", "Goosefish", "Cod", "Mackerel", "Pollock", "White hake", "Silver hake","Herring")
  names(fig.names) <- sp.names
seasons <- c('Spring','Fall')
# ID'g which species had length structured ssp runs (bc inputs differ)
length.struct <- rep(0,length(sp.names))
  names(length.struct) <- sp.names
length.struct[c('SDog','WSk')] <- 1
length.struct.sp <- names(length.struct[length.struct==1])


# Organize the basics (sp/ages/yrs/ints)
source( paste(my.programs, "R/9Species Model/Results Files/Organizing.Basics.R", sep="") , local=TRUE)

# List templates
list.bysp <- vector('list',nsp)
  names(list.bysp) <- sp.names
list.bysp.season <- lapply(list.bysp,function(x){
                                     x<-vector('list',length(seasons))
                                     names(x) <- seasons
                                     x  })

# Organize multispecies outputs into lists
source( paste(my.programs, "R/9Species Model/Results Files/Organizing.MSP.Outputs.R", sep="") , local=TRUE)

# Organize data from ssp runs into lists
source( paste(my.programs, "R/9Species Model/Input Files/Organize_Individ_SpData.R", sep="") )

# Examine fits to input data
source( paste(my.programs, "R/9Species Model/Results Files/Fits.To.InputData.R", sep="") , local=TRUE)

# Plot predicted indices
source( paste(my.programs, "R/9Species Model/Results Files/Examine.Predicted.Indices.R", sep="") , local=TRUE)

# Analyize correlation file
source( paste(my.programs, "R/9Species Model/Results Files/Analyze Corr File.R", sep="") , local=TRUE)

setwd(sp9.dir)
save.image("OrganizedResults.RData")
