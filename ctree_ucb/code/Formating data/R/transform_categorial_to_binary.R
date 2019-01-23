transform_categorial_to_binary <- function(listCategorial=0 , listInteger=0,dt){             #have to precice column data 
  dt<- as.data.frame(dt)
  
  if( listCategorial==0 ) return(dt)
  
  if( listInteger==0 ){
    if(length(listCategorial)==1) return(vect_to_matrix_transform(dt[,listCategorial]))  ###A changer
    return(cbind(model.matrix(~ . + 0, data=dt[,listCategorial] , 
                              contrasts.arg = lapply(dt[,listCategorial] , contrasts, contrasts=FALSE))))
    
  }
  
  return(cbind(dt[,listInteger],model.matrix(~ . + 0, data=dt[,listCategorial] , 
                                             contrasts.arg = lapply(dt[,listCategorial] , contrasts, contrasts=FALSE))))
}


######### Transformation
vect_to_matrix_transform  <- function(vec) {
  vec <- as.character(vec)
  model.matrix(~vec-1)
}
