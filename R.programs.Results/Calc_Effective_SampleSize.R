function(obs,pred)  {
  #both observed and predicted must be matrices of [year X age]    
  obs[apply(obs==0,1,all),] <- NA
    #If all elements in a year == 0, set the elements to NA (bc age samples were not collected)
  pred[apply(is.na(obs),1,all),] <- NA
  num <- pred*(1-pred)
    # num <- pred.cprop[[i]]*(1-pred.cprop[[i]])
  den <- ( obs - pred )^2
    # den <- ( obs.cprop[[i]]-pred.cprop[[i]] )^2
  result <- rowSums(num) / rowSums(den)
  result
}
