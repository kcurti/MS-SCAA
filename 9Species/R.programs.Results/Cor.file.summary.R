function (path,filename,mod.corr)
{
# This function reads in the admb correlation file (*.cor) and then outputs tables
      #of moderate, high and maximum correlations according to the specified thresholds
# path = path to the folder with the cor file
# filename = '*.cor'
# mod.corr = threshold correlation value to use for the "moderate" matrix

# example:
    #path <- "C:/My_Programs/ADMB/3species_MCSim"
    #filename <- "MCSim.cor"
    #mod.corr <- 0.6

setwd(path)

#Reading in the correlation file
corr<-read.table(filename, header=TRUE, skip=1, fill=TRUE)

#Extracting the variable names
name<-as.character(corr$name)

#Counting the number of times a variable is repeated (name2) and combining that 
  #count with the original variable name (comb.name) so that each new variable
  #name is unique
comb.name <- rep("",length(name))
name2 <- rep(NA,length(name))
name2[1] <- 1
for (i in 2:length(name))
	{
	if (name[i]!= name[i-1]) name2[i]<- 1  else name2[i] <- name2[i-1]+1
	}
for (i in 1:length(name))
	{
	comb.name[i] <-  paste(name[i],name2[i])
	}

#Subsetting the correlation file by selecting just the correlation 
    #matrix from the other columns (corr2)
corr2<-corr[,5:(dim(corr)[2]-1)]  
corr2[1:10,1:10]
dim(corr2)

#Calculating the max and minimum correlations observed between parameter pairs
corr3<- corr2
for (i in 1:dim(corr3)[1])
	{
	for (j in 1:dim(corr3)[2])
		{
		if(is.na(corr2[i,j])=="FALSE")
			{
			if (corr3[i,j]==1) 
				{
				corr3[i,j] = 0
				}
			}
		}
	}

#Defining a matrix and filling them with a list of the pairs of 
  #parameters and corresponding correlation values for which 
  #rho > 0.8 (moderate)

moderate<-matrix(NA,(dim(corr2)[1]^2)/2,3)

#counters
k<-1
d<-1

for (i in 1:dim(corr2)[1])
	{
	for (j in 1:dim(corr2)[2])
		{
		if(is.na(corr2[i,j])=="FALSE")
			{
			if (abs(corr2[i,j]) >= mod.corr & abs(corr2[i,j]) < 1.0 )   
				{
				moderate[k,1] <- comb.name[i] 
				moderate[k,2] <- comb.name[j]
				moderate[k,3] <- corr2[i,j]
				k<- k+1 
				}
			}
		}
	}


#Sorting the "moderate" matrix by the correlation value
o<-order(abs(as.numeric(moderate[,3])),decreasing=T)
moderate<-moderate[o,]

#Removing all of the lines that are NA's
#moderate[1:10,]
moderate<-moderate[!is.na(moderate[,3]),]
#moderate

if (class(moderate)=="character") {moderate <- t(matrix(moderate))}

if( dim(moderate)[[1]]>0 ) {max.corr <- moderate[1,3]}  else {
  max.corr <- NULL
  moderate <- NULL}

write.table(moderate,"ModerateCorrelations.txt",sep="\t",col.names=c("Var1","Var2","corr.coeff"))
write.table(max.corr,"MaxCorrelation.txt",sep="\t",col.names=c("corr.coeff"))

result <- list(max.corr,moderate)
  names(result) <- c("max.corr","moderate.corr")
result
}






