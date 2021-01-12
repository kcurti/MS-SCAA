###########################################
#Organizes food habits data for input into msp model
###########################################


# Organize species interactions; Determine pred, prey and nint

split.ints <- strsplit(int.names,"-")
pred.names <- unlist(lapply(split.ints,function(x){x[1]}))
prey.names <- unlist(lapply(split.ints,function(x){x[2]}))

pred <- sp.order[pred.names]
prey <- sp.order[prey.names]
nint<-length(pred)


# Import FH data

Sp9.FH.orig <- read.csv('C:\\Users\\Kiersten L Curti\\Documents\\Dissertation\\Data\\NEFSC Food Habits Data\\GBank_AnnualTimeStep\\9SpeciesModel\\Sp9Diet_ForR.csv')
Sp9.FH.orig[1:10,]
  
Sp9.FH.full.list <-  split(Sp9.FH.orig,Sp9.FH.orig$'Sp_No')
  names(Sp9.FH.full.list) <- as.vector( unique(Sp9.FH.orig$'Predator'))

FH.sp.names <- c(sp.names,'Other')
names(FH.sp.names) <- c(sp.names,'Other')
FH.sp.names['WH'] <- 'WHake'
FH.sp.names['SH'] <- 'SHake'
names(FH.sp.names) <- NULL

# Select just FH rows; Divide by 100 to convert percentages to proportions
Sp9.FH.list <- lapply(Sp9.FH.full.list, function(x){x <- as.matrix(x[,FH.sp.names])/100} )





						
