Tracking=function(indv, cam, camerror, numsteps) {
  recs = NULL
  for(s in 1:numsteps){
    x = indv[s+1]
    y = indv[s+2]
    
    #look for overlapping camera
    #does indv location match camera location in x coord
    t = cam[cam[,2]==x,,drop=F] 
    if(nrow(t)>0){
      #if yes, does indv location match camera location in y coord
      t = t[t[,3]==y,,drop=F]   
      if(nrow(t)>0){
        #if x and y locations match, apply error rate and record individual camera captured
        rec = sample(c(indv[1], 0), 1, prob=c((1-camerror), camerror))
        recs = c(recs, rec)
      }
    }
  }
  return(recs)
}
