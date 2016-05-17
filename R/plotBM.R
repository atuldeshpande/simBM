#' Plot Brownian Motion
#'
#' This function plots 1-D and 2-D Brownian motion
#'
#' @param x a vector of real numbers representing points in brownian motion
#' @param dim in {1,2} for the dimension of Brownian Motion
#' @author Atul Deshpande
#' @importFrom graphics plot
#' @export
plotBM <- function(x,dim){
  if (dim==2)
    plot(x,type='l',xlab = 'Dimension 1',ylab = 'Dimension 2')
  if (dim==1)
    plot(x,type='l',ylab = 'Dimension 1')
}
