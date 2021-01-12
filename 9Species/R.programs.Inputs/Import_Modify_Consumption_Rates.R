################################################################################
# Import consumption estimates for input into msp model
# Consumption estimates were initially estimated in SAS
# Consumption rates are manually modified below (to deal with low sample sizes)
################################################################################

# Recall:  CB ratios (init_matrix CB(1,nsp,1,nage)): 

fh.prim.dir <- c('C:\\Users\\Kiersten L Curti\\Documents\\Dissertation\\Data\\NEFSC Food Habits Data\\GBank_AnnualTimeStep\\')


#### Grosslein CB estimates used in Chapter 1 #####
# CB.Ch1.list <- list.bysp
  # CB.Ch1.list[['Cod']] <- c(5,3.9,3.3,3,2.7,2.5,2.3,2.2,2.1,1.95)
  # CB.Ch1.list[['SH']]  <- c(6.5,5.2,4.7,4.2,3.9,3.566667)
  # CB.Ch1.list[['Her']] <- c(5.8064,5.2,4.7,4.5,4.2,4.05)

CB.nbins <- 3

CB.bin1 <- list.bysp
CB.bin3 <- list.bysp

CB.avg <- list.bysp

fh.prim.dir <- c('C:\\Users\\Kiersten L Curti\\Documents\\Dissertation\\Data\\NEFSC Food Habits Data\\GBank_AnnualTimeStep')

# Fill lists
for (i in 1:nsp)  {
  sp <- sp.names[i]
  full.sp <- full.sp.names[sp]  
  CB.bin1[[sp]] <- read.csv(paste(fh.prim.dir,full.sp,'CB_Estimates_1Yrbins.csv',sep="//"), header=T,na.strings="")
  CB.bin3[[sp]] <- read.csv(paste(fh.prim.dir,full.sp,'CB_Estimates_3Yrbins.csv',sep="//"), header=T,na.strings="")
  }
  
# Function to reduce columns and sory by PdAge, YrBin
organize.cb <- function(x) {
  x <- x[,c("Species","PdAge","YrBin","AvgPdWt_g","S_nstom","F_nstom","S_consumpt2","F_consumpt2","An_Consumpt2","CB2")]
  if(unique(na.omit(x$Species)) != 'Spiny Dogfish')  {x <- x[x$PdAge!=0,]};  # remove 0-age class for all species but spiny dogfish
  x <- x[with(x,order(PdAge,YrBin)),];
  x;
  }
CB.bin1 <- lapply(CB.bin1, organize.cb)
CB.bin3 <- lapply(CB.bin3, organize.cb)

# Manually go through each species and select CB estimates to use
   # Need to average over years, ages where number of stomachs are less than 10

all.var <- c('S_nstom','F_nstom','S_consumpt2','F_consumpt2')
spr.var <- c('S_nstom','S_consumpt2')
fall.var <- c('F_nstom','F_consumpt2')



####################################################################

# SDog: 
sp <- 'SDog'
tmp3 <- CB.bin3[[sp]]
tmp1 <- CB.bin1[[sp]]
nstom.tmp3 <- cbind(tmp3$S_nstom,tmp3$F_nstom)
tmp3.lown <- tmp3[apply(nstom.tmp3,1,min)<10,]
ages.lown <- unique(tmp3.lown$PdAge)
# Age 0: Leave as is 
# Age 1, YrBin3: Use average spring consumption
   age <- 1; bin <- 3;
   tmp3[tmp3$PdAge==age,]
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 2, Yrbin 3: Use average spring consumption
   age <- 2; bin <- 3;
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 15, Yrbin 1: Use average spring and fall consumption
   age <- 15; bin <- 1;
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
# Age 16, Yrbin 1: Use average spring and fall consumption
   age <- 16; bin <- 1;
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
# Age 17, Yrbin 1: Use average spring and fall consumption
   age <- 17; bin <- 1;
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
# Age 18, Yrbin 1: Use average spring and fall consumption
   age <- 18; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
# Age 18, Yrbin 3: Use average fall consumption
   age <- 18; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 19, Yrbin 1: Use average spring and fall consumption
   age <- 19; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
# Age 21, Yrbins 1 and 3: Use average spring and fall consumption
   age <- 21; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
   age <- 21; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
# Age 23: Yrbin 1: Use average spring and fall consumption
   age <- 23; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var] <- tmp1[tmp1$PdAge==age,all.var]
# Age 23: Yrbin 3: Use average fall consumption
   age <- 23; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 26: Yrbin 1: Use average spring consumption
   age <- 26; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 26: Yrbin 3: Use average fall consumption
   age <- 26; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]

# Recalculate CB from new seasonal consumption estimates
tmp3$'An_Consumpt2' <- tmp3$'S_consumpt2' + tmp3$'F_consumpt2'
tmp3$'CB2' <- tmp3$'An_Consumpt2' / tmp3$'AvgPdWt_g'

# Ages 27, 28, 29: Use weighted average of ages 26 and 30 for each year bin
ages27.29 <- data.frame(matrix(NA,nrow=9,ncol=ncol(tmp3)))
  colnames(ages27.29) <- colnames(tmp3)
  ages27.29[,'Species'] <- 'Spiny Dogfish'
  ages27.29[,'PdAge'] <- rep(27:29,each=3)
  ages27.29[,'YrBin'] <- rep(rep(1:3),3)
age1 <- 26
age2 <- 30
tmp3.sub <- tmp3[(tmp3$PdAge==age1 | tmp3$PdAge==age2),] 
tot.nstom <- split(apply(tmp3.sub[,c('S_nstom','F_nstom')],1,sum),tmp3.sub$YrBin)
cb.tmp <- split(tmp3.sub[,'CB2'],tmp3.sub$YrBin)
for (i in 1:3)  {
  cb.tmp1 <- sum(tot.nstom[[i]]*cb.tmp[[i]])/sum(tot.nstom[[i]])
  ages27.29[ages27.29$YrBin==i,'CB2'] <- cb.tmp1
  }

# Ages 24, 25: Use weighted average of ages 23 and 26 for each year bin
nage.tmp <- 2
ages24.25 <- data.frame(matrix(NA,nrow=(3*nage.tmp),ncol=ncol(tmp3)))
  colnames(ages24.25) <- colnames(tmp3)
	ages24.25[,'Species'] <- 'Spiny Dogfish'
	ages24.25[,'PdAge'] <- rep(24:25,each=3)
	ages24.25[,'YrBin'] <- rep(rep(1:3),nage.tmp)
age1 <- 23
age2 <- 26
tmp3.sub <- tmp3[(tmp3$PdAge==age1 | tmp3$PdAge==age2),] 
tot.nstom <- split(apply(tmp3.sub[,c('S_nstom','F_nstom')],1,sum),tmp3.sub$YrBin)
cb.tmp <- split(tmp3.sub[,'CB2'],tmp3.sub$YrBin)
for (i in 1:3)  {
  cb.tmp1 <- sum(tot.nstom[[i]]*cb.tmp[[i]])/sum(tot.nstom[[i]])
  ages24.25[ages24.25$YrBin==i,'CB2'] <- cb.tmp1
  }

# Age 20: Use weighted average of ages 19 and 21 for each year bin
nage.tmp <- 1
ages20 <- data.frame(matrix(NA,nrow=(3*nage.tmp),ncol=ncol(tmp3)))
  colnames(ages20) <- colnames(tmp3)
	ages20[,'Species'] <- 'Spiny Dogfish'
	ages20[,'PdAge'] <- rep(20,each=3)
	ages20[,'YrBin'] <- rep(rep(1:3),nage.tmp)
age1 <- 19
age2 <- 21
tmp3.sub <- tmp3[(tmp3$PdAge==age1 | tmp3$PdAge==age2),] 
tot.nstom <- split(apply(tmp3.sub[,c('S_nstom','F_nstom')],1,sum),tmp3.sub$YrBin)
cb.tmp <- split(tmp3.sub[,'CB2'],tmp3.sub$YrBin)
for (i in 1:3)  {
  cb.tmp1 <- sum(tot.nstom[[i]]*cb.tmp[[i]])/sum(tot.nstom[[i]])
  ages20[ages20$YrBin==i,'CB2'] <- cb.tmp1
  }

# Age 22: Use weighted average of ages 21 and 23 for each year bin
nage.tmp <- 1
ages22 <- data.frame(matrix(NA,nrow=(3*nage.tmp),ncol=ncol(tmp3)))
  colnames(ages22) <- colnames(tmp3)
	ages22[,'Species'] <- 'Spiny Dogfish'
	ages22[,'PdAge'] <- rep(22,each=3)
	ages22[,'YrBin'] <- rep(rep(1:3),nage.tmp)
age1 <- 21
age2 <- 23

tmp3.sub <- tmp3[(tmp3$PdAge==age1 | tmp3$PdAge==age2),] 
tot.nstom <- split(apply(tmp3.sub[,c('S_nstom','F_nstom')],1,sum),tmp3.sub$YrBin)
cb.tmp <- split(tmp3.sub[,'CB2'],tmp3.sub$YrBin)
for (i in 1:3)  {
  cb.tmp1 <- sum(tot.nstom[[i]]*cb.tmp[[i]])/sum(tot.nstom[[i]])
  ages22[ages22$YrBin==i,'CB2'] <- cb.tmp1
  }

# Merge fudged ages with tmp3
tmp3 <- rbind(tmp3,ages27.29,ages24.25,ages20,ages22)
# Sort by PdAge then year bin
tmp3 <- tmp3[with(tmp3,order(PdAge,YrBin)),];

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# WSk: 
sp <- 'WSk'
tmp3 <- CB.bin3[[sp]]
tmp1 <- CB.bin1[[sp]]
nstom.tmp3 <- cbind(tmp3$S_nstom,tmp3$F_nstom)
tmp3.lown <- tmp3[apply(nstom.tmp3,1,min)<10,]
ages.lown <- unique(tmp3.lown$PdAge)


# Age 1: Yrbins 1 and 3: Use average spring and fall consumption
   age <- 1; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   age <- 1; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
# Age 2: Yrbins 1: Use average spring and fall consumption
   age <- 2; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
# Age 15: Yrbins 3: Use average spring consumption
   age <- 15; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 16: YrBin3: Use average spring consumption
   age <- 16; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 17: Yrbin3: Use average spring consumption
   age <- 17; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 18: YrBin 3: Use average spring and fall consumption
   age <- 18; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
# Age 19: YrBin 3: Use average Spring consumption
   age <- 19; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 20:  YrBin 1: Use average fall consumption
   age <- 20; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 20:  YrBin 3: Use average spring and fall consumption
   age <- 20; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,'Species'] <- 'Winter Skate'
# Age 21:  YrBin 3: Use average spring and fall consumption
   age <- 21; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,'Species'] <- 'Winter Skate'

# Recalculate CB from new seasonal consumption estimates
tmp3$'An_Consumpt2' <- tmp3$'S_consumpt2' + tmp3$'F_consumpt2'
tmp3$'CB2' <- tmp3$'An_Consumpt2' / tmp3$'AvgPdWt_g'

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# Goosefish: Not enough stomachs: use time-invariant but willl need to fuss with time-invariant as well
sp <- 'Goose'
tmp1 <- CB.bin1[[sp]]
tot.nstom <- tmp1$S_nstom + tmp1$F_nstom
  names(tot.nstom) <- tmp1$PdAge
ages.lown <- names(tot.nstom)[tot.nstom<10]
# Age 1: Use average of age1 and age2
age <- '1'
age1 <- '1'
age2 <- '2'
tot.age1 <- tot.nstom[age1]*tmp1[tmp1$PdAge==age1,'CB2'] 
tot.age2 <- tot.nstom[age2]*tmp1[tmp1$PdAge==age2,'CB2']
tmp1[tmp1$PdAge==age,'CB2'] <- (tot.age1 + tot.age2) / (tot.nstom[age1] + tot.nstom[age2])
# Age 7: Use average of ages 6-8
age1 <- '6'
age2 <- '7'
age3 <- '8'
tot.age1 <- tot.nstom[age1]*tmp1[tmp1$PdAge==age1,'CB2'] 
tot.age2 <- tot.nstom[age2]*tmp1[tmp1$PdAge==age2,'CB2']
tot.age3 <- tot.nstom[age3]*tmp1[tmp1$PdAge==age3,'CB2']
cb.tmp <- (tot.age1 + tot.age2 + tot.age3) / (tot.nstom[age1] + tot.nstom[age2] + tot.nstom[age3])
tmp1[tmp1$PdAge=='7','CB2'] <- cb.tmp
# Ages 8, 9 and 10: Use average of ages 8-10
age1 <- '8'
age2 <- '9'
age3 <- '10'
tot.age1 <- tot.nstom[age1]*tmp1[tmp1$PdAge==age1,'CB2'] 
tot.age2 <- tot.nstom[age2]*tmp1[tmp1$PdAge==age2,'CB2']
tot.age3 <- tot.nstom[age3]*tmp1[tmp1$PdAge==age3,'CB2']
cb.tmp <- (tot.age1 + tot.age2 + tot.age3) / (tot.nstom[age1] + tot.nstom[age2] + tot.nstom[age3])
tmp1[tmp1$PdAge==age1,'CB2'] <- cb.tmp
tmp1[tmp1$PdAge==age2,'CB2'] <- cb.tmp
tmp1[tmp1$PdAge==age3,'CB2'] <- cb.tmp

# Duplicate for year bins
tmp1
tmpY2 <- tmp1
  tmpY2$YrBin <- 2
tmpY3 <- tmp1
  tmpY3$YrBin <- 3  
tmp3 <- rbind(tmp1,tmpY2,tmpY3)  
# Sort by PdAge then year bin
tmp3 <- tmp3[with(tmp3,order(PdAge,YrBin)),];

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# Cod
sp <- 'Cod'
tmp3 <- CB.bin3[[sp]]
tmp1 <- CB.bin1[[sp]]
nstom.tmp3 <- cbind(tmp3$S_nstom,tmp3$F_nstom)
tmp3.lown <- tmp3[apply(nstom.tmp3,1,min)<10,]
ages.lown <- unique(tmp3.lown$PdAge)
# Age 7: Yrbins 1 and 3: Use average fall consumption
   age <- 7; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
   age <- 7; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 8: Yrbins 1, 2 and 3: Use average fall consumption
   age <- 8; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
   age <- 8; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
   age <- 8; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age <- 9, Yr bins 1:3:  Average fall consumption = average of ages 8-10 fall consumption
   age1 <- '8'
	 age2 <- '9'
	 age3 <- '10'	
	 nstom.age1 <- tmp1[tmp1$PdAge==age1,'F_nstom'] 
  	 tot.age1 <- nstom.age1*tmp1[tmp1$PdAge==age1,'F_consumpt2'] 
   nstom.age2 <- tmp1[tmp1$PdAge==age2,'F_nstom']  
  	 tot.age2 <- nstom.age2*tmp1[tmp1$PdAge==age2,'F_consumpt2']
   nstom.age3 <- tmp1[tmp1$PdAge==age3,'F_nstom']
  	 tot.age3 <- nstom.age3*tmp1[tmp1$PdAge==age3,'F_consumpt2']
  tmp3[tmp3$PdAge=='9','F_consumpt2'] <- (tot.age1 + tot.age2 + tot.age3) / (nstom.age1 + nstom.age2 + nstom.age3)
  tmp3[tmp3$PdAge=='9','F_nstom']        <- (nstom.age1 + nstom.age2 + nstom.age3)
# Age 9, YrBin 3:  Use average spring consumption
   age <- 9; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age 10: Yrbins 1, 2: Use average fall consumption
   age <- 10; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
   age <- 10; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 10: Yrbin 3 Use average spring and fall consumption
   age <- 10; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,'Species'] <- 'Atlantic Cod'

# Recalculate CB from new seasonal consumption estimates
tmp3$'An_Consumpt2' <- tmp3$'S_consumpt2' + tmp3$'F_consumpt2'
tmp3$'CB2' <- tmp3$'An_Consumpt2' / tmp3$'AvgPdWt_g'

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# Mack
sp <- 'Mack'
tmp1 <- CB.bin1[[sp]]

# Calculate weighted-average of time-invariant CB ratios for ages 4+ 
tmp1.sub <- tmp1[tmp1$PdAge>=4 & tmp1$PdAge!=8,]
totn.sub <- tmp1.sub$'S_nstom' + tmp1.sub$'F_nstom' 
cb.tmp <- sum(totn.sub*tmp1.sub$CB2) / sum(totn.sub)
tmp1[tmp1$PdAge>=4,'CB2'] <- cb.tmp

# Fill in species for age-8
tmp1[tmp1$PdAge=='8','Species'] <- 'Atlantic Mackerel'
# Create a line for age-9
age8.tmp <- tmp1[tmp1$PdAge=='8',]
age8.tmp[,'PdAge'] <- 9
# Merge with original matrix
tmp1 <- rbind(tmp1,age8.tmp)
# Sort by predator age
tmp1 <- tmp1[with(tmp1,order(PdAge)),];

# Duplicate for year bins
tmp1
tmpY2 <- tmp1
  tmpY2$YrBin <- 2
tmpY3 <- tmp1
  tmpY3$YrBin <- 3  
tmp3 <- rbind(tmp1,tmpY2,tmpY3)  
# Sort by PdAge then year bin
tmp3 <- tmp3[with(tmp3,order(PdAge,YrBin)),];

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# Pollock
sp <- 'Pol'
tmp3 <- CB.bin3[[sp]]
tmp1 <- CB.bin1[[sp]]
nstom.tmp3 <- cbind(tmp3$S_nstom,tmp3$F_nstom)
tmp3.lown <- tmp3[apply(nstom.tmp3,1,min)<10,]
ages.lown <- unique(tmp3.lown$PdAge)

# Age 1: Yrbins1-3: Use average spring and fall consumption
   age <- 1; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,'Species'] <- 'Pollock'
   age <- 1; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   age <- 1; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
# Age 2: Yrbins1: Use average fall consumption
   age <- 2; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 4: Yrbins1: Use average fall consumption
   age <- 4; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 5: Yrbins1: Use average fall consumption
   age <- 5; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 6: Yrbins 1&2 : Use average fall consumption
   age <- 6; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
   age <- 6; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,fall.var] <- tmp1[tmp1$PdAge==age,fall.var]
# Age 6: Yrbins 3 : Use average spring and fall consumption
   age <- 6; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
# Age 9: Yrbins 2 and 3: Use average spring and fall consumption
   age <- 9; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,'Species'] <- 'Pollock'
   age <- 9; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
# Recalculate CB from new seasonal consumption estimates (before dealing with ages 7 and 8 time-invariant consumption)
tmp3.sub1 <- tmp3[tmp3$PdAge!=7 & tmp3$PdAge!=8,]
tmp3.sub1$'An_Consumpt2' <- tmp3.sub1$'S_consumpt2' + tmp3.sub1$'F_consumpt2'
tmp3.sub1$'CB2' <- tmp3.sub1$'An_Consumpt2' / tmp3.sub1$'AvgPdWt_g'

# Age 7 and 8: Use average (of ages 7 and 8) time-invariant consumption
   tmp1.sub <- tmp1[tmp1$PdAge==7 | tmp1$PdAge==8,]
   totn.sub <- tmp1.sub$'S_nstom' + tmp1.sub$'F_nstom' 
   cb.tmp <- sum(totn.sub*tmp1.sub$CB2) / sum(totn.sub)
   tmp1.sub[,'CB2'] <- cb.tmp
   # Duplicate for year bins
   tmp1.sub
   tmpY2 <- tmp1.sub
     tmpY2$YrBin <- 2
   tmpY3 <- tmp1.sub
     tmpY3$YrBin <- 3  
   tmp1.sub.rep <- rbind(tmp1.sub,tmpY2,tmpY3)  

# Merge with remaining tmp3 (tmp3.sub1)
tmp3 <- rbind(tmp1.sub.rep,tmp3.sub1)
# Sort by PdAge then year bin
tmp3 <- tmp3[with(tmp3,order(PdAge,YrBin)),];

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# White hake
sp <- 'WH'
tmp3 <- CB.bin3[[sp]]
tmp1 <- CB.bin1[[sp]]
nstom.tmp3 <- cbind(tmp3$S_nstom,tmp3$F_nstom)
tmp3.lown <- tmp3[apply(nstom.tmp3,1,min)<10,]
ages.lown <- unique(tmp3.lown$PdAge)

# Age1: YrBins 1, 2 and 3: Use average spring consumption
   age <- 1; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
   age <- 1; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
   age <- 1; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age3: Yrbin 3: Use average spring consumption
   age <- 3; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age4: YrBin 2: Use average spring consumption
   age <- 4; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age4: Yrbin 3: Use average spring consumption
   age <- 4; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age5: Yrbins 1&2:  Use average spring consumption
   age <- 5; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
   age <- 5; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age5: Yrbin 3: Use average spring and fall consumption
   age <- 5; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]

# Recalculate CB from new seasonal consumption estimates (before dealing with ages 6 and 7 time-invariant consumption)
tmp3.sub1 <- tmp3[tmp3$PdAge!=6 & tmp3$PdAge!=7,]
tmp3.sub1$'An_Consumpt2' <- tmp3.sub1$'S_consumpt2' + tmp3.sub1$'F_consumpt2'
tmp3.sub1$'CB2' <- tmp3.sub1$'An_Consumpt2' / tmp3.sub1$'AvgPdWt_g'

# Age 6 and 7: Use average (ages 6-7) time-invariant consumption
    # Examined average of ages 5-7, and only varies from this average by 0.02
    # So used the above due to ease of calculations
   tmp1.sub <- tmp1[tmp1$PdAge==6 | tmp1$PdAge==7,]
   totn.sub <- tmp1.sub$'S_nstom' + tmp1.sub$'F_nstom' 
   cb.tmp <- sum(totn.sub*tmp1.sub$CB2) / sum(totn.sub)
   tmp1.sub[,'CB2'] <- cb.tmp
   # Duplicate for year bins
   tmp1.sub
   tmpY2 <- tmp1.sub
     tmpY2$YrBin <- 2
   tmpY3 <- tmp1.sub
     tmpY3$YrBin <- 3  
   tmp1.sub.rep <- rbind(tmp1.sub,tmpY2,tmpY3)  

# Merge with remaining tmp3 (tmp3.sub1)
tmp3 <- rbind(tmp1.sub.rep,tmp3.sub1)
# Sort by PdAge then year bin
tmp3 <- tmp3[with(tmp3,order(PdAge,YrBin)),];

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# Silver hake
sp <- 'SH'
tmp3 <- CB.bin3[[sp]]
tmp1 <- CB.bin1[[sp]]
nstom.tmp3 <- cbind(tmp3$S_nstom,tmp3$F_nstom)
tmp3.lown <- tmp3[apply(nstom.tmp3,1,min)<10,]
ages.lown <- unique(tmp3.lown$PdAge)

# Age5: Yrbins 2&3:  Use average spring consumption
   age <- 5; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
   age <- 5; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]
# Age6: Yrbins 1&2:  Use average spring and fall consumption
   age <- 6; bin <- 1
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
   age <- 6; bin <- 2
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,all.var]  <- tmp1[tmp1$PdAge==age,all.var]
# Age 6: YrBin 3:  Use average spring consumption
   age <- 6; bin <- 3
   tmp3[tmp3$PdAge==age & tmp3$YrBin==bin,spr.var] <- tmp1[tmp1$PdAge==age,spr.var]

# Recalculate CB from new seasonal consumption estimates
tmp3$'An_Consumpt2' <- tmp3$'S_consumpt2' + tmp3$'F_consumpt2'
tmp3$'CB2' <- tmp3$'An_Consumpt2' / tmp3$'AvgPdWt_g'

# Save to list
CB.avg[[sp]] <- tmp3


####################################################################

# Herring
sp <- 'Her'
tmp3 <- CB.bin3[[sp]]
tmp1 <- CB.bin1[[sp]]
nstom.tmp3 <- cbind(tmp3$S_nstom,tmp3$F_nstom)
tmp3.lown <- tmp3[apply(nstom.tmp3,1,min)<10,]
ages.lown <- unique(tmp3.lown$PdAge)
# Age 1: YrBin 1: Use average consumption (Need to create a line)
age1.tmp <- tmp1[tmp1$PdAge=='1',]
# Age 2: YrBin 1: Use average consumption (Need to create a line)
age2.tmp <- tmp1[tmp1$PdAge=='2',]
# Age 3: YrBin 1: Use average consumption (Need to create a line)
age3.tmp <- tmp1[tmp1$PdAge=='3',]
# Age 4: YrBin 1: Use average consumption (Need to create a line)
age4.tmp <- tmp1[tmp1$PdAge=='4',]

# Recalculate CB from new seasonal consumption estimates (before dealing with ages 6 and 7 time-invariant consumption)
tmp3.sub1 <- tmp3[tmp3$PdAge!=5 & tmp3$PdAge!=6,]
tmp3.sub1 <- tmp3.sub1[!apply(is.na(tmp3.sub1), 1, any),] 
tmp3.sub1 <- rbind(tmp3.sub1,age1.tmp,age2.tmp,age3.tmp,age4.tmp)

tmp3.sub1$'An_Consumpt2' <- tmp3.sub1$'S_consumpt2' + tmp3.sub1$'F_consumpt2'
tmp3.sub1$'CB2' <- tmp3.sub1$'An_Consumpt2' / tmp3.sub1$'AvgPdWt_g'

# Age 5 and 6: Use average (ages 4,5,6) time-invariant consumption
   tmp1.sub <- tmp1[tmp1$PdAge>=4,]
   totn.sub <- tmp1.sub$'S_nstom' + tmp1.sub$'F_nstom' 
   cb.tmp <- sum(totn.sub*tmp1.sub$CB2) / sum(totn.sub)
   tmp1.sub <- tmp1[tmp1$PdAge>=5,]
   tmp1.sub[,'CB2'] <- cb.tmp
   # Duplicate for year bins
   tmp1.sub
   tmpY2 <- tmp1.sub
     tmpY2$YrBin <- 2
   tmpY3 <- tmp1.sub
     tmpY3$YrBin <- 3  
   tmp1.sub.rep <- rbind(tmp1.sub,tmpY2,tmpY3)  

# Merge with remaining tmp3 (tmp3.sub1)
tmp3 <- rbind(tmp3.sub1,tmp1.sub.rep)
# Sort by PdAge then year bin
tmp3 <- tmp3[with(tmp3,order(PdAge,YrBin)),];

# Save to list
CB.avg[[sp]] <- tmp3

