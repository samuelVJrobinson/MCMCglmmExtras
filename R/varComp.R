#' Variance components for the random effects of an MCMCglmm model.
#'
#' Calculates proportion of variance component taken up by each random effect. Returns posterior mode from kernel density estimation, as well as upper and lower CIs for each random effect.
#'
#' NOTE: Use units=T with caution if running multinomial models (unit variance may be unestimable, and should be fixed at a constant).
#' @param mod MCMCglmm model
#' @param units Use unit ("residual") variance?
#' @export
#' @examples
#' load(data)
#' mod1<-MCMCglmm(Count~x,random=~y+z,data=data,family='poisson')
#' varComp(mod1)

varComp=function(mod,units=T) { #Tabulates variance components of random effects for MCMCglmm models
  VCV=mod$VCV
  if(units==F) VCV=VCV[,c(1:ncol(VCV)-1)] #Strips out "units" column (for binomial models)
  round(data.frame(mode=posterior.mode(VCV/rowSums(VCV)),HPDinterval(VCV/rowSums(VCV))),3)
}
