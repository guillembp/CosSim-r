library("devtools")
library(roxygen2)

#' 1. Cosine Similarity
#'
#' @description This function calculates the row similarity of a nxm matrix using the cosine pairwise projection
#' @param matrix_cs only required parameter. If required similarity by columns, transpose the input matrix.
#' @keywords netowrks similarity recommendation
#' @export
#' @examples matrix(data = c(1,1,1,0,1,1,0,1), ncol = 2)

CosSim <- function(matrix_cs){
  cols <- ncol(matrix_cs)
  loop <- function(ix){
    Q = matrix_cs[,ix[1]]
    W = matrix_cs[,ix[2]]
    if(Q==0||W==0){
      return(0)
    }
    return(sum(Q*W)/sqrt(sum(Q^2)*sum(W^2)))
  }
  cmb <- expand.grid(i=1:cols, j=1:cols)
return(matrix(data = apply(X = cmb, MARGIN = 1, FUN = loop), nrow = cols, ncol = cols))
}
