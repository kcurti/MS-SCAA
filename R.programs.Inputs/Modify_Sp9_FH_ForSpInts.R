
################################################################################
# Modify_Sp9_FH_ForSpInts.R

# Modify observed FH data (Sp9.FH.list) to set %Wt's = 0 for any species interaction that is not explicitly modeled
################################################################################

  
Sp9.ModFH.list <- Sp9.FH.list

# Recall:
  # lapply(Sp9.ModFH.list,class)
  # names(Sp9.ModFH.list)
  # FH.sp.names

# Confirm that rows in Sp9.FH.list sum to 1
unique(unlist(lapply(Sp9.FH.list,rowSums)))

full.sp.names <- rep(NA,length(sp.names))
  names(full.sp.names) <- sp.names
  full.sp.names['SDog'] <- 'SpinyDogfish'
  full.sp.names['WSk'] <- 'WinterSkate'
  full.sp.names['Goose'] <- 'Goosefish'
  full.sp.names['Cod'] <- 'Cod'
  full.sp.names['Mack'] <- 'Mackerel'
  full.sp.names['Pol'] <- 'Pollock'
  full.sp.names['WH'] <- 'WhiteHake'
  full.sp.names['SH'] <- 'SilverHake'
  full.sp.names['Her'] <- 'Herring'

# Binary matrix to ID modeled species interactions
pdpy.matrix <- array(0,dim=c(nsp,nsp))
  # recall that sizepref.names = the predator species
  rownames(pdpy.matrix) <- sp.names           # Preds
  colnames(pdpy.matrix)  <- sp.names            # Prey
for (i in 1:nint)  {
  pdpy.matrix[ pred.names[i],prey.names[i] ] <- 1
  }

# Function sets obs FH for non-prey == 0, and adds any observed %W's for non-prey to the "other food" term
Modify.fh <- function(x,non.prey) {       
                           #  x <- FH.tmp3Sp[['SilverHake']]
                           #  non.prey <- FH.sp.names[c(-10,-9,-8)]
                       other.sp <- x[rowSums(x)<=100,non.prey]
                       other.plus <- rowSums(other.sp)
                       x[rowSums(x)<=100,'Other'] <- x[rowSums(x)<=100,'Other'] + other.plus
                       x[rowSums(x)<=100,non.prey] <- 0
                       x
                       }
# Apply Modify.fh to each predator species
for (i in 1:nrow(pdpy.matrix))
  { 
  pd <- rownames(pdpy.matrix)[i]
  nonpy <- mod.sp.names[names(pdpy.matrix[pd,][pdpy.matrix[pd,]==0])]
  Sp9.ModFH.list[[ full.sp.names[pd] ]] <- Modify.fh(Sp9.ModFH.list[[full.sp.names[pd]]], nonpy)
  } 
  
# Confirm that rows in Sp9.ModFH.list still sum to 1
unique(unlist(lapply(Sp9.ModFH.list,rowSums)))
  


