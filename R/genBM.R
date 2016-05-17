#' Generate Brownian Motion
#'
#' This function generates 1-D and 2-D Brownian motion
#'
#' @param vector_size a positive integer value for the number of brownian motion points
#' @param dim in {1,2} for the dimension of Brownian Motion
#' @param seed an integer value to seed the brownian motion
#' @return a numeric vector of stages of Brownian motion dimensions vector_size x dim
#' @author Atul Deshpande
#' @export
#' @importFrom stats rnorm
#' @examples
#' genBM(3,1,1)
#' genBM(300,2)
genBM<-
  function(vector_size,dim,seed)
  {
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
    if (!missing('seed'))
    {
      set.seed(seed, kind = NULL, normal.kind = NULL);
    }
      updates <- array(rnorm(vector_size*dim, mean = 0, sd = 1),c(vector_size,dim))
      simBM<-matrix(0,vector_size,dim);
      for (i in 1:dim)
        simBM[,i] <- cumsum(updates[,i])
    return(simBM)
  }
