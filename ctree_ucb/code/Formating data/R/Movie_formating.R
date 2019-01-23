

movie <- read.csv('Data/ml-latest-small/movies.csv')
tag <- read.csv('Data/ml-latest-small/tags.csv')
rating <- read.csv('Data/ml-latest-small/ratings.csv')
links  <- read.csv('Data/ml-latest-small/links.csv')


movie$genre_Adventure <- 0
movie$genre_Action <- 0
movie$genre_Comedy <- 0
movie$genre_Drama <- 0
movie$genre_Film_Noir <- 0
movie$genre_Romance <- 0
movie$genre_Sci_Fi <- 0
movie$genre_Thriller <- 0
movie$genre_Animation <- 0
movie$genre_Western <- 0
movie$genre_IMAX<- 0
movie$genre_Documentary <- 0
movie$genre_War <- 0
movie$genre_Musical <- 0
movie$genre_Children<- 0
movie$genre_Fantasy <- 0
movie$genre_Horror <- 0
movie$genre_Crime <- 0
movie$genre_Mystery <- 0

for(i in 1:nrow(movie)){
  temp <- movie$genres[i]
  
  if(grepl(temp , "Adventure") == TRUE)  movie$genre_Adventure[i] = 1
  if(grepl(temp , "Comedy") == TRUE)  movie$genre_Comedy[i] = 1
  if(grepl(temp , "Drama") == TRUE)  movie$genre_Drama[i] = 1
  if(grepl(temp , "Romance") == TRUE)  movie$genre_Romance[i] = 1
  if(grepl(temp , "Sci-Fi") == TRUE)  movie$genre_Sci_Fi[i] = 1
  if(grepl(temp , "Thriller") == TRUE)  movie$genre_Sci_Fi[i] = 1
  if(grepl(temp , "Animation") == TRUE)  movie$genre_Animation[i] = 1
  if(grepl(temp , "Action") == TRUE)  movie$genre_Action[i] = 1
  if(grepl(temp , "Film-Noir") == TRUE)  movie$genre_Film_Noir[i] = 1
  if(grepl(temp , "Western") == TRUE)  movie$genre_Western[i] = 1
  if(grepl(temp , "IMAX") == TRUE)  movie$genre_IMAX[i] = 1
  if(grepl(temp , "War") == TRUE)  movie$genre_War[i] = 1
  if(grepl(temp , "Musical") == TRUE)  movie$genre_Musical[i] = 1
  if(grepl(temp , "Children") == TRUE)  movie$genre_Children[i] = 1
  if(grepl(temp , "Fantasy") == TRUE)  movie$genre_Fantasy[i] = 1
  if(grepl(temp , "Horror") == TRUE)  movie$genre_Horror[i] = 1
  if(grepl(temp , "Crime") == TRUE)  movie$genre_Crime[i] = 1
  if(grepl(temp , "Mystery") == TRUE)  movie$genre_Mystery[i] = 1
}


summary(as.factor(rating$userId))

my_df <- movie
my_df$user_547  <- NA
my_df$user_195 <- NA
my_df$user_384 <- NA
my_df$user_463   <- NA
my_df$user_all  <- NA


#my_ranting <- merge.data.frame(movie,rating,by="movieId",all.x = TRUE)

#547
temp_data_ranting  <- subset.data.frame(rating,userId == 547 )
for(i in 1:nrow(my_df)){
  temp_data_ranting_2 <- subset.data.frame(temp_data_ranting ,  temp_data_ranting$movie== my_df[i,]$movieId )
  
  if(nrow(temp_data_ranting_2 )==1){
    my_df$user_547[i] =  temp_data_ranting_2$rating[1]
  }else{
    my_df$user_547[i] =   mean(rating[rating$movieId ==  my_df[i,]$movieId ,]$rating)
    
    #handle movies without ranking
    if( is.nan(my_df$user_547[i]) == TRUE)  my_df$user_547[i] = 0
    
  }
}


#195
temp_data_ranting  <- subset.data.frame(rating,userId == 195 )
for(i in 1:nrow(my_df)){
  temp_data_ranting_2 <- subset.data.frame(temp_data_ranting ,  temp_data_ranting$movie== my_df[i,]$movieId )
  
  if(nrow(temp_data_ranting_2 )==1){
    my_df$user_195[i] =  temp_data_ranting_2$rating[1]
  }else{
    my_df$user_195[i] =  mean(rating[rating$movieId ==  my_df[i,]$movieId ,]$rating)
    #handle movies without ranking
    if( is.nan(my_df$user_195[i]) == TRUE)  my_df$user_195[i] = 0
  }
}


#384
temp_data_ranting  <- subset.data.frame(rating,userId == 384 )
for(i in 1:nrow(my_df)){
  temp_data_ranting_2 <- subset.data.frame(temp_data_ranting ,  temp_data_ranting$movie== my_df[i,]$movieId )
  
  if(nrow(temp_data_ranting_2 )==1){
    my_df$user_384[i] =  temp_data_ranting_2$rating[1]
  }else{
    my_df$user_384[i] =  mean(rating[rating$movieId ==  my_df[i,]$movieId ,]$rating)
    #handle movies without ranking
    if( is.nan(my_df$user_384[i]) == TRUE)  my_df$user_384[i] = 0
  }
}

#463 
temp_data_ranting  <- subset.data.frame(rating,userId == 463  )
for(i in 1:nrow(my_df)){
 temp_data_ranting_2 <- subset.data.frame(temp_data_ranting ,  temp_data_ranting$movie== my_df[i,]$movieId )

  if(nrow(temp_data_ranting_2 )==1){
   my_df$user_463 [i] =  temp_data_ranting_2$rating[1]
  }else{
    my_df$user_463 [i] =  mean(rating[rating$movieId ==  my_df[i,]$movieId ,]$rating)

#handle movies without ranking
    if( is.nan(my_df$user_463 [i]) == TRUE)  my_df$user_463 [i] = 0
  }
}


#all
for(i in 1:nrow(my_df)){
  
    my_df$user_all[i] =   mean(rating[rating$movieId ==  my_df[i,]$movieId ,]$rating)
    
    #handle movies without ranking
    if( is.nan(my_df$user_all[i]) == TRUE)  my_df$user_all[i] = 0
    
  
}


dt <- my_df[,c(4:22)]

visitorReward <- my_df[,c(23:27)]
boxplot(visitorReward)

rm(list=setdiff(ls(), c("dt","visitorReward")))

arm_best <- vector()
for( i in 1:nrow(visitorReward) ) arm_best[i] <- which.max(visitorReward[i,])
summary(as.factor(arm_best))

for( i in 1:nrow(visitorReward) ) visitorReward[i,]  <- 100*visitorReward[i,]




