#' simBM
#' 
#' Generate and Plot Brownian Motion
#'
#' This function generates and plots 1-D and 2-D Brownian motion
#'
#' @param vector_size a positive integer value for the number of brownian motion points
#' @param dim =1,2 for the dimension of Brownian Motion. Default = 1
#' @param seed an integer value to seed the brownian motion. Default no seed
#'  
#' @export
#' @return a numeric vector of stages of Brownian motion dimensions vector_size x dim
#' @author Atul Deshpande
#' @examples
#' simBM(3,1,1)
#' simBM(300,2)
simBM<- 
  function(vector_size,dim,seed){
  if (missing('seed'))
  { if (!testInteger(vector_size) || !testInteger(dim))
      stop('Error: All inputs expected to be integers'); 
    if (missing('dim'))
    {
      if (!testInteger(vector_size))
        stop('Error: All inputs expected to be integers'); 
      dim<-1
    }
    if (dim>2)
      stop('Error: This code only creates up to 2D BMs');
  }
  else{
    if (!testInteger(vector_size) || !testInteger(dim) || !testInteger(seed))
      stop('Error: All inputs expected to be integers');
  }
  simBM1<-genBM(vector_size,dim,seed)
  plotBM(simBM1,dim)
  return(simBM1)
}
