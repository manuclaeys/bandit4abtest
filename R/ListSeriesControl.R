#'ListSeriesControl
#'
#'Control if the ListSeries elements are in on the dataframe colomn names
#'Print a message and stop if this condition is not respected.
#'Else return TRUE.
#'
#'@param listSeriesList of characters vectors
#'@param dt  Dataframe of integer numeric or factor values
#'@return Logical value?c
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 35, .05)
#'## Define a dataframe of rewards
#'dt <- as.data.frame(cbind(K1,K2))
#'## Define a dataframe of context
#'l <- c("K1","K2")
#'ListSeriesControl(listSeriesList = l,dt=dt)
#'
#'@export
ListSeriesControl  <- function(listSeriesList,dt) {
  #Match size controle

  if (all (listSeriesList %in% colnames(dt)) == FALSE )   {
    stop("list of time series to be used and column names do not match")
    return (FALSE)
  }
  return (TRUE)
}
