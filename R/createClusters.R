#'createClusters
#'
#'Create cluster based on a specified clustering method
#'
#'@param listSerie  List of characters (Colom Name)
#'@param dt Dataframe of time series
#'@param method Method for clustering (DBA or HCLUST), DBA is set by default
#'@param listKCentroids  List of Integer (Number of centroid)
#'@param plotCentroids Boolean (optional) If you want to see the centroids
#'@param plotClusters Boolean (optional) If you want to see the clusters
#'@param  maxIter Integer. Maximum number of allowed iterations for partitional/fuzzy clustering.
#'
#'@return Integer value
#'
#'@examples
#'set.seed(100000)
#'alpha_list <- c(1,2,3)
#'beta_list <- c(0.5,0.1,-0.2)
#'theta_list <- c(0.8,0.2,0.5)
#'y <- as.data.frame(c(1))
#'colnames(y) = "ID"
#'temp=1
#'for (j in 1:100){
#'  for (i in 1:length(alpha_list)){
#'    n = sample(1:1000,1)
#'    t <- 1:n
#'    ts <- alpha_list[i] + beta_list[i] * t + arima.sim(list(ma = theta_list[i]), n = length(t))
#'    y[temp, "time_series"][[1]] <- list(ts)
#'    y[temp, "cluster"][[1]] <- i
#'    y$ID[temp] = temp
#'    temp = temp +1
#'  }
#'}
#'y <- y[sample(nrow(y)),]
#'obj <- createClusters(listSerie = c("time_series") , dt = y , method = "DBA" , listKCentroids=c(3) , plotCentroids = TRUE , plotClusters = TRUE , maxIter = 10L )
#'table(obj$dt$cluster, obj$dt$clustertime_series)
#'@import dtwclust doParallel cluster
#'
#'@export
createClusters <- function(listSerie = colnames(dt) , dt , method = "DBA" , listKCentroids , plotCentroids = TRUE , plotClusters = TRUE  , maxIter = 10L) {
  #check if ListSeries element are in colnames dataframe
  ListSeriesControl(listSeriesList = listSerie ,dt=dt )

  #check the number of centroid
  listKCentroidsControl(listKCentroids, listSerie)

  #check if methode is DBA or HCLUST
  if((method %in% c("DBA","HCLUST")) == FALSE)  {
    stop("select DBA or HCLUST method")
    return (FALSE)
  }

  #fast clustering
  library(dtwclust)
  library(doParallel)
  cl <- makeCluster(detectCores())
  invisible(clusterEvalQ(cl, library(dtwclust)))
  registerDoParallel(cl)

  #object to keep clustering informations
  pc.dba <- c()
  #init
  j=1
  for(i in listSerie){
    print(i)

    # HCLUST
    if(method == "HCLUST"){
      ts <-   tsclust( dt[[i]], k = listKCentroids[j], type = "h",
                       seed = 3251,
                       distance = "dtw",  trace = TRUE )
    }

    # DBA
    if(method == "DBA"){
      ts <-   tsclust( dt[[i]], k = listKCentroids[j],
                       centroid = "dba",
                       seed = 3251, trace = TRUE
                       ,control = partitional_control(nrep = 1L,iter.max = maxIter ))
    }

    pc.dba <-c(pc.dba, ts)
    j=j+1

  }

  # Stop parallel workers
  stopCluster(cl)

  # Return to sequential computations. This MUST be done after stopCluster()
  registerDoSEQ()

  if(plotCentroids == TRUE )  for(i in 1:length(listSerie)) plot(pc.dba[i][[1]], type = "centroids", main=as.character(listSerie[i]))
  if(plotClusters == TRUE )  for(i in 1:length(listSerie)) plot(pc.dba[i][[1]],   main=as.character(listSerie[i]))


  #Add clusters
  for(i in 1:length(listSerie)) dt[[paste("cluster",listSerie[i],sep = "")]] <- 0
  for(i in 1:length(listSerie)) dt[[paste("cluster",listSerie[i],sep = "")]] <- pc.dba[i][[1]]@cluster


  return(list('clust_obj'=pc.dba,'dt'=dt))

}
