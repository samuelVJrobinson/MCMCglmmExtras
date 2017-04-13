#' Deviance Information Criteria (DIC)
#'
#' Extracts DIC from a individual or set of MCMCglmm models. If a set of models is provided, provides a table of DIC differences between candidate models.
#' @param m1 MCMCglmm model
#' @param ... Other MCMCglmm models
#' @export
#' @examples
#' DIC(mod1)
#' DIC(mod1,mod2)

DIC=function(m1,...){
  modlist=list(m1,...)
  dicvals=unlist(lapply(modlist,'[[',c('DIC')))
  fixeffs=as.character(lapply((lapply(modlist,'[[','Fixed')),'[[','formula'))
  raneffs=as.character(lapply((lapply(modlist,'[[','Random')),'[[','formula'))
  data.frame(Fixed=fixeffs,Random=raneffs,DIC=dicvals,deltaDIC=dicvals-min(dicvals))
}
