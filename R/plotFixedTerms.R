#' 95\% credible intervals for posterior distibution.
#'
#' \code{plotFixedTerms} plots 95\% credible intervals for posterior distibution
#' using \code{ggplot2}
#' @param mod1 MCMCglmm model
#' @examples
#' plotFixedTerms(mod1)

plotFixedTerms=function(mod1){ #Convenience function to plot 95% distributions for MCMCglmm fixed effects
  require(ggplot2)
  dat=as.data.frame(summary(mod1)$solutions)
  dat$params=as.factor(rownames(dat))
  colnames(dat)[c(2,3)]=c('lwr','upr')
  ggplot(dat,aes(x=params,y=post.mean,ymax=upr,ymin=lwr))+geom_pointrange()+labs(x='Fixed Effects',y='Posterior Mean')+geom_hline(yintercept=0,colour='red')
}
