#' @title  Overbooking ticket calculator
#'
#' @param N the total number of sits on the plain
#' @param gamma probability of overbooking
#' @param p the probability that a passenger will show
#'
#' @return return a list with the results for the tickets with the initial input parameters. It also plots the objective vs n function used to solve the problem (normal approx and binomial)
#' @export
#' @import graphics
#' @examples
#' ntickets(N=400,gamma=0.02,p=0.95)
#'
ntickets <- function(N,gamma,p)
{
  'appropriate discrete distribution '
  fb <- function(n,No,gammao,po) {No-stats::qbinom(1-gammao,n,po)}
  nrootb <- stats::uniroot(fb,c(N,N+N/4),tol = 0.00001,No = N,gammao = gamma,po = p)
  xo = N:(N+N/4)
  ybi = fb(xo,N,gamma,p)
  c <- data.frame(xo,ybi)


  'normal approximation'

  fn <- function(n,No,gammao,po) {No-stats::qnorm(1-gammao + stats::pnorm(0.5,n*po,sqrt(n*po*(1-po))),n*po,sqrt(n*po*(1-po)))}
  nrootn <- stats::uniroot(fn,c(N,N+N/4),tol = 0.00001,No = N,gammao = gamma, po = p)
  yn = fn(xo,N,gamma,p)
  z <- data.frame(xo, yn)

  'printing plots'

  g = ggplot2::ggplot(data = c, mapping = ggplot2::aes(x=xo,y=ybi))
  g = g + ggplot2::geom_line()+ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) + ggplot2::geom_vline(ggplot2::aes(xintercept = nrootb$root))+ggplot2::labs(title ="Objective Vs n discrete",x = "n",y = "objective")
  print(g)


  g = ggplot2::ggplot(data = z, mapping = ggplot2::aes(x=xo,y=yn))
  g = g+ ggplot2::geom_point()+ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) + ggplot2::geom_vline(ggplot2::aes(xintercept = nrootn$root))+ggplot2::labs(title ="Objective Vs n continuous",x = "n",y = "objective")
  print(g)

  r <- list(nrootb$root,nrootn$root,N,p,gamma)
  names(r) <- c('nd','nc','N','p','Gamma')
  return(r)
}






