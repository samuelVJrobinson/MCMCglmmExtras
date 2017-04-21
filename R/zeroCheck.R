#' Expected zeros from a Poisson model
#'
#' Posterior predictive checks for zero-inflation (MCMCglmm course notes pg. 104). Runs simulation NITT times, using individual realizations from the model to simulate a poisson distribution. The number of zeros from each simulation are plotted along with the actual number of zeros from the dataset, in order to decide if the model requires a zero-inflation term ('zipoisson')
#' @param mod1 MCMCglmm model
#' @param dat Data frame used to fit \code{mod1}
#' @examples
#' load(data)
#' mod1<-MCMCglmm(Count~x,data=data,family='poisson')
#' zeroCheck(mod1,data)

zeroCheck=function(mod,dat) {
  require(MCMCglmm)
  require(ggplot2)
  try(if(is.null(mod$X)) stop("Needs fixed effect design matrix (use saveX=T in MCMCglmm arguments)"))
  nz=1:nrow(mod$Sol) #Number of samples to hold
  oz=sum(dat[as.character(mod$Fixed$formula[2])]==0) #Actual number of 0s in the dataset
  for (i in 1:nrow(mod$Sol)) {
    pred.l <- rnorm(nrow(dat), (mod$X %*% mod$Sol[i,])@x, sqrt(mod$VCV[i])) #Predicted mean
    nz[i] <- sum(rpois(nrow(dat), exp(pred.l)) == 0)} #Expected number under poisson
  qplot(nz,xlab='Expected number of zeros')+geom_vline(aes(xintercept=oz))
  #Expected zeros, given the overdispersed poisson model
  }
