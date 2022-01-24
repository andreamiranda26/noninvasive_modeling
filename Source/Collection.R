Collection = function(pop, gen, generror, landscape){
  genIDs = NULL
  
  #iterate over collection events
  for(g in 1:nrow(gen)){
    #selecting sampling location
    x = gen[g,2]
    y = gen[g,3]
    
    #find paths that intersect sample location, examining one individual at a time
    for(r in 1:nrow(pop)){
      indv = pop[r,]
      ID = indv[1]
      
      #convert steps to 1 per row
      indv = indv[2:length(indv)]
      indv = data.frame(x=indv[seq(1,length(indv), 2)], y=indv[seq(2,length(indv), 2)])
      
      #find overlapping locations in x dimension
      indv = indv[indv$x==x,,drop=F]
      
      #find overlapping locations in y dimensio
      indv = indv[indv$y==y,,drop=F]
      
      if(nrow(indv)>0){
        ids = sample(c(ID, 0), 1, prob=c((1-generror), generror))
        genIDs = c(genIDs, ids)
      }
      if(nrow(indv)==0){
        genIDs = c(genIDs, -1) #no sample was found/genotyped
      }
    }
  }
  #return list of identified individuals
  return(genIDs)
}