Cameras <- function(numcamera, landscape){ 
  #set camera IDs
  ID= seq(1,numcamera,1) 
  
  #randomly assign locations
  xloc <- sample(1:landscape, numcamera, replace=T) #takes a camera x location out of the landscape
  yloc <- sample(1:landscape, numcamera, replace=T) #takes a y location of the camera on landscape
  
  #combine data
  cam <- cbind(ID, xloc,yloc) #creates that whole cam object
  
  return(cam) #this will spit it out when run in complex model
}

