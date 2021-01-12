# Calc.multinomial.residuals.R
  #Function to calculate multinomial residuals

function(obs,pred,obs.tot,p) 
  {  #Note: obs and pred must be year x age; and obs.tot must be 1 x year
  res <- matrix(NA,nrow(obs),ncol(obs))
  for (t in 1:nrow(obs))  
    {
    if(obs.tot[t] != 0)  { res[t,] <- as.matrix((obs[t,] + p)*log(pred[t,] + p)) }
    }
  res
  }
