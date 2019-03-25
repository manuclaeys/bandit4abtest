#'control_data_missing
#'
#'Control data for bandit.
#'Check in a dataframe if there is some missing values
#'Print a message if it's not respected.
#'Else return TRUE.
#'
#'@param visitorReward Dataframe of integer or numeric values
#'
#'@return Logical value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'## Control
#'control_data_missing(visitorReward)
#'visitorReward[1,1]= NA
#'## Control
#'control_data_missing(visitorReward)
#'
#'@export
control_data_missing <- function(visitorReward){
  #no missing data
  if(sum(colSums(is.na(visitorReward))) > 0){
    stop("missing data in arm results database")
    return(FALSE)
  }
  return(TRUE)
}


