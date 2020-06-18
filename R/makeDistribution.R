#' Builds an empiric distribution from a sample
#'
#' Make a continuous distribution from a empiric sample.
#' @usage makeDistribution(sample, minimum = min(sample)-sd(sample), maximum = max(sample)+sd(sample), smooth=F, ...)
#' @param sample data array to make the distribution.
#' @param minimum,maximum values to be considered as minimum and maximum to make the distribution.
#' @param smooth boolean, if its true then it would smooth data before make the distribution. 
#' @param ... alrguments to pass on method \code{smooth.spline}.
#' @details The default minimum is min(sample)-sd(sample), this is made to improve the results, but it can be changed by 
#' the real minimum declarating it in the inputs. Same for maximum.
#' @return Returns a list with four functions.\cr
#' \code{pfun(q)} Cumulative distribution function.\cr
#' \code{qfun(p)} Quantile function of the distibution.\cr
#' \code{rfun(n)} Generates random deviates.\cr
#' \code{dfun(x)} Gives the density. It doesn't work by now, but will be improved in next updates.
#' @author Daniel Fischer
#' @examples 
#' data1=rnorm(10000)
#' data2=runif(10000) #take 2 different samples  
#' dist1=makeDistribution(data1,minimum=min(data1),maximum=max(data1))
#' dist2=makeDistribution(data2,minimum=min(data2),maximum=max(data2)) #make distributions of them
#' quantiles=seq(0,1,by=0.1) #vector of deciles
#' probabilities= seq(0,1,by=0.001) #vector of probabilities
#' 
#' #Compare the empiric distributions with base version. 
#' require(ggplot2)
#' a=qplot(quantiles,pnorm(quantiles),main="Real pnorm")
#' b=qplot(probabilities,qnorm(probabilities),main="Real qnorm")
#' c=qplot(probabilities,dnorm(probabilities),main="Real dnorm")
#' d=qplot(quantiles,dist1$pfun(quantiles),main="makeDitribution-pnorm")
#' f=qplot(probabilities,dist1$qfun(probabilities),main="makeDitribution-qnorm")
#' g=qplot(probabilities,dist1$dfun(probabilities),main="makeDitribution-dnorm")  
#' h=qplot(quantiles,punif(quantiles),main="Real punif")
#' i=qplot(probabilities,qunif(probabilities),main="Real qunif")
#' j=qplot(probabilities,dunif(probabilities),main="Real dunif")
#' k=qplot(quantiles,dist2$pfun(quantiles),main="makeDitribution-punif")
#' l=qplot(probabilities,dist2$qfun(probabilities),main="makeDitribution-qunif")
#' m=qplot(probabilities,dist2$dfun(probabilities),main="makeDitribution-dunif")  
#' multiplot(a,b,c,d,f,g,h,i,j,k,l,m,cols=4)
#' @export
makeDistribution = function(sample, minimum = min(sample)-sd(sample) , maximum = max(sample)+sd(sample) ,smooth = F,...){
  if(min(sample) > minimum) {sample = c(minimum,sample)}
  if(max(sample) < maximum) {sample = c(maximum,sample)}
  sample[sample < minimum] = minimum
  sample[sample > maximum] = maximum
  dataset = data.frame(sample_values = sample[order(sample)] )
  dataset$cumulative = seq(from = 0,to = 1, length = nrow(dataset))

  if(smooth ){
    smooted = smooth.spline(dataset$sample_values,dataset$cumulative,...)$y
    smooted[1] = 0
    smooted[nrow(dataset)] = 1
    smooted[smooted<0]  = 0
    smooted[smooted>1]  = 1
    smooted = sapply(1:length(smooted), function(n) max(smooted[1:n]))
    dataset$cumulative = smooted
  }

  pfun = approxfun(dataset$sample_values,dataset$cumulative,yleft = 0,yright = 1)
  qfun = approxfun(dataset$cumulative,dataset$sample_values)
  rfun = function(n){qfun(runif(n))}
  dfun = function(x){grad(pfun,x)}

  return(list(pfun = pfun, qfun = qfun, rfun = rfun, dfun = dfun))
}
