#'listKCentroidsControl
#'
#'Control if the listSeries and listKCentroidsControl corresponds
#'Print a message and stop if this condition is not respected.
#'Else return TRUE.
#'
#'@param listSeriesList List of characters vectors
#'@param listSeriesList List of integer vector
#'@return Logical value
#'
#'@examples
#'listKCentroids = c(1,2,3)
#'listSeriesList = c("Type 1","Type 2")
#'listKCentroidsControl(listKCentroids, listSeriesList)
#'@export
listKCentroidsControl  <- function(listKCentroids,listSeriesList ) {
  #Match size controle

  if ( (length(listSeriesList) != length(listKCentroids)) | (typeof(listKCentroids) != "double" ) )   {
    stop("Something wrong with listKCentroidsControl and listSeriesList")
    return (FALSE)
  }
  return (TRUE)
}
