DIC=function(m1,...){ #Convenience function to extract DIC values from MCMCglmm models
  modlist=list(m1,...)
  dicvals=unlist(lapply(modlist,'[[',c('DIC')))
  fixeffs=as.character(lapply((lapply(modlist,'[[','Fixed')),'[[','formula'))
  raneffs=as.character(lapply((lapply(modlist,'[[','Random')),'[[','formula'))
  data.frame(Fixed=fixeffs,Random=raneffs,DIC=dicvals,deltaDIC=dicvals-min(dicvals))
}
