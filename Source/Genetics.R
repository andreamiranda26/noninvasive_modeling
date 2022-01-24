Genetics <- function(numgen, landscape){ 
  #set camera IDs
  ID= seq(1,numgen,1) 
  
  #randomly assign locations
  xloc <- sample(1:landscape, numgen, replace=T) #takes a camera x location out of the landscape
  yloc <- sample(1:landscape, numgen, replace=T) #takes a y location of the camera on landscape
  
  #combine data
  gen <- cbind(ID, xloc,yloc) #creates that whole gen object
  
  return(gen) 
}