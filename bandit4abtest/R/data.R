#' abtest1
#'
#' A/B test on 6216 visitors of a website (Learnset:30\% Trainset 70\%).
#' Dataset from a frequentist ab test conducted in 2018 by a merchant website.
#' Visitors to the site have seen version A or version B only.
#'  30% of the dataset is used for a pre-processing step (see CTREEUCB).
#'  The data is not modified (missing values).
#'  set learn_size = 1865
#'  The remaining 70% of the dataset is dedicated to the online phase.
#'  It is necessary to have a reward for all variations.
#'  Missing values are replaced by bootstrap with the Leave at Least One method
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{countryID}{ID of visitor's country, factor}
#'   \item{latitude}{last position of visitor, recorded before the test, numeric}
#'   \item{longitude}{last position of visitor, recorded before the test, numeric}
#'   \item{langID}{ID of visitor language, factor}
#'   \item{name}{visitor's OS, factor}
#'   \item{device}{ID of visitor device, factor}
#'   \item{userAgent}{visitor's user agent, factor}
#'   \item{A}{visitor's reward with A version of a website}
#'   \item{B}{visitor's reward with A version of a website}
#'   ...
#' }
#' @source \url{http://abtasty.com/}
#' @examples
#'  try(data(package = "bandit4abtest") )
#'  load(abtest1)
"abtest1"

#' abtest2
#'
#' A/B test on 6216 visitors of a website (Learnset:100\% Trainset 100\%).
#' Dataset from a frequentist ab test conducted in 2018 by a merchant website.
#' Visitors to the site have seen version A or version B only.
#'  100\% of the dataset is used for a pre-processing step (see CTREEUCB).
#'  The data is not modified (missing values).
#'  set learn_size = 6216
#'  100\% of the same dataset is dedicated to the online phase.
#'  It is necessary to have a reward for all variations.
#'  Missing values are replaced by bootstrap with the Leave at Least One method
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{countryID}{ID of visitor's country, factor}
#'   \item{latitude}{last position of visitor, recorded before the test, numeric}
#'   \item{longitude}{last position of visitor, recorded before the test, numeric}
#'   \item{langID}{ID of visitor language, factor}
#'   \item{name}{visitor's OS, factor}
#'   \item{device}{ID of visitor device, factor}
#'   \item{userAgent}{visitor's user agent, factor}
#'   \item{A}{visitor's reward with A version of a website}
#'   \item{B}{visitor's reward with A version of a website}
#'   ...
#' }
#' @source \url{http://abtasty.com/}
#' @examples
#'  try(data(package = "bandit4abtest") )
#'  load(abtest2)
"abtest2"
