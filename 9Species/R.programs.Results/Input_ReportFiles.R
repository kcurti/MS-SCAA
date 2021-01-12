#Final ssp and 3species runs for initial parameter estimates/data weightings, etc
setwd(paste(my.programs,"ADMB/SingleSpecies/Cod/",cod.run,sep=""))
Cod.rep<-reptoRlist("Cod.rep")
Cod.rep$ofv
Cod.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/SilverHake/",sh.run,sep=""))
SH.rep<-reptoRlist("SilverHake.rep")
SH.rep$ofv
SH.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/Herring/",her.run,sep=""))
Her.rep<-reptoRlist("Her.rep")
Her.rep$ofv
Her.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/Mackerel/",mack.run,sep=""))
Mack.rep<-reptoRlist("Mack.rep")
Mack.rep$ofv
Mack.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/Goosefish/",goose.run,sep=""))
Goose.rep<-reptoRlist("Goose.rep")
Goose.rep$ofv
Goose.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/WhiteHake/",wh.run,sep=""))
WH.rep<-reptoRlist("WhiteHake.rep")
WH.rep$ofv
WH.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/Pollock/",pol.run,sep=""))
Pol.rep<-reptoRlist("Pollock.rep")
Pol.rep$ofv
Pol.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/SpinyDogfish/",sdog.run,sep=""))
SDog.rep<-reptoRlist("SpinyDog.rep")
SDog.rep$ofv
SDog.rep$M1.1
setwd(paste(my.programs,"ADMB/SingleSpecies/WinterSkate/",wsk.run,sep=""))
WSk.rep<-reptoRlist("WinterSkate.rep")
WSk.rep$ofv
WSk.rep$M1.1

setwd(paste(my.programs,"ADMB/3Species/",sp3.run,sep=""))
sp3.rep <-reptoRlist("3Species.rep")

