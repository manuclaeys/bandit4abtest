#' Generate a S Matrix
#'
#' Returns a matrix initialized to 0 of dimension K by 2.
#' Allows to save the average empirical reward (first line) and number of trials (second line) of each arm into a matrix S.
#' Require a numercial values to define the number of possible arm
#'
#'@param x Integer variable
#'
#'@return Numeric matrix
#'
#'@examples
#'K = 2
#'GenerateMatrixS(K)
#'
#'@export
GenerateMatrixS <- function(x) {
  S <- matrix(rep(0,2*x), nrow = 2, ncol = x)
  colnames(S) <- paste('bandit', 1:x)
  rownames(S) <- c("average reward","trials")
  return (S)
}
