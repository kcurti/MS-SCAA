path= paste(my.programs,"R/Functions/Cor.file.summary.R",sep="")
  corr.file<-dget(path);

setwd(sp9.dir)

base.name <- c('9Species')
# Analyzing the correlation file
corr.results <- corr.file(getwd(),paste(base.name,"cor",sep="."),0.8)
