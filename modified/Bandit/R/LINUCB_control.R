LINUCBControl <- function(dt, visitorReward, K) {
  LINUCBControlK(visitorReward = visitorReward, K = K)
  LINUCBControlData(dt = dt, visitorReward = visitorReward, K = K)
}

LINUCBControlData <- function(dt, visitorReward, K) {
  #  contextual data size controle
  try(if (nrow(dt) == 0) stop("empty contextual data"))
  #  reward data size controle
  try(if (nrow(visitorReward) == 0) stop("empty reward data"))
  #  Match size controle
  try(if (nrow(dt) != nrow(visitorReward)) stop("number of row in contextual data and rewards data are not equals"))
  #  Type controle visitor reward
  listRewardPossible <- c("double", "integer")
  for (i in 1:ncol(visitorReward)) {
    try(
      if (length(intersect(typeof(visitorReward[, i]), listRewardPossible)) == 0) 
        stop(paste("The", i, "colomn of reward data is not a double of integer data", sep = " ")))
  }
  #  Type controle contextual data
  listContextPossible <- c("double", "integer")
  for (i in 1:ncol(dt)) {
    try(
      if (length(intersect(typeof(dt[, i]), listContextPossible)) == 0) 
        stop(paste("The", i, "colomn of context data is not a double of integer data", sep=" ")))
  }
  #  Reward data type identicals control
  typeOfReward <- sapply(visitorReward, typeof)
  try(
    if (length(unique(type_of_reward)) > 1) 
      stop("reward data type are not identicals "))
  #  no missing data
  try(
    if (sum(colSums(is.na(visitorReward))) > 0)
      stop("missing data in arm results database"))
  return(TRUE)
}

LINUCBControlK <- function(visitorReward, K) {
  #  arm must be superior to 2
  try(if (K<2) stop("arm must be superior or equal to 2"))
  #  each arm get a result
  try(if (ncol(visitorReward) != K) stop("each arm need a result"))
  return(TRUE)
}





