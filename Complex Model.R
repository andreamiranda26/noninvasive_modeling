#complex model run for ABM Class Fall 2021
#setwd("~/GitHub/ABM-Course")
setwd("~/Desktop/ABM-Course-main/")

#to make sure it is the right place you should do session then set up working directory
directory = getwd()
outdir    = paste(directory,"/Output/", sep="")
source(paste(directory, "/Source/FunctionSourcer.R", sep =''))

#list of parameters
landscape.V = 50                         #landscape size
numindiv.V  = c(50)                      #start off with a number of individuals 
numsteps.V  = 500                        #number of steps individuals will take 
move.V      = 0.95                       #Likelihood of individuals moving to the next cell 95% of the time they will move 
numcamera.V = c(25)                      #number of cameras placed on the landscape
camerror.V  = c(0.001, 0.01, 0.1, 0.2)   #prop of incorrect IDs using camera
numgen.V    = c(25)                      #number of genetic samples taken over time
generror.V  = c(0.001, 0.01, 0.1, 0.2)   #prop of incorrect IDs using genetic samples
reps        = 100                        #number of replicates for each set of variables

#set up parameter values to run the model with
parameters= expand.grid(landscape.V,numindiv.V,numsteps.V,move.V,numcamera.V,camerror.V,numgen.V,generror.V,reps) #this creates data frame for combination of variables
colnames(parameters) = c("landscape","numindiv","numsteps","move","numcamera","camerror","numgen","generror","reps")

#run model, each row of parameters is a different set
for(p in 1:nrow(parameters)){
  for(r in 1:reps){
    landscape = parameters$landscape[p]
    numindiv  = parameters$numindiv[p]
    numsteps  = parameters$numsteps[p]
    move      = parameters$move[p]
    numcamera = parameters$numcamera[p]
    camerror  = parameters$camerror[p]
    numgen    = parameters$numgen[p]
    generror  = parameters$generror[p]
    
    #initialize landscape 
    land = LandscapeInit(landscape)
    #image(land)
    
    #initialize individuals on landscape
    pop = Pop(numindiv, landscape) #individuals placed on the landscape 
    #plot(pop)
    
    #iterate over individuals and set up their movement path for duration of monitoring period
    pathways = NULL
    plot(-100,-100, xlim=c(1,landscape), ylim=c(1,landscape))
    for(i in 1:nrow(pop)){
      #isolate individual of interest
      n = pop[i,,drop=FALSE] 
      #the i means iterates
      movepath = Move(landscape,n,numsteps,move,numcamera)
      lines(movepath[seq(1,numsteps,2)], movepath[seq(2,numsteps,2)])
      pathways = rbind(pathways, movepath) # record path in a single object for all individuals 
    }
    pop = cbind(pop[,1] ,pathways)
    
    #place cameras on landscape
    cam = Cameras(numcamera, landscape)
    points(cam[,2], cam[,3], pch=19, col="firebrick3")
    
    #extract camera image history for individuals (incorporating error)
    records = NULL
    for(i in 1:nrow(pop)){
      indv = pop[i,]
      hits = Tracking(indv, cam, camerror, numsteps)
      records = c(records, hits)
    }
    
    #extract camera record data
    nrecords = length(records)
    nerrors  = length(records[records==0])
    nunique  = length(unique(records[records>0]))
    camdata = c(nrecords, nerrors, nunique)
    
    #try to collect DNA samples desired number of times reached (incorporating error)
    gen = Genetics(numgen, landscape)
    points(gen[,2], gen[,3], pch=19, col="dodgerblue3")
    genIDs = Collection(pop, gen, generror, landscape)
    
    #extract genetic record data
    genIDs = genIDs[genIDs>=0]
    ngens = length(genIDs)
    ngenerrors  = length(genIDs[genIDs==0])
    ngenunique  = length(unique(genIDs[genIDs>0]))
    gendata = c(ngens, ngenerrors, ngenunique)
    
    #record data to file
    towrite = c(r,landscape,numindiv,numsteps,move,numcamera,camerror,numgen,generror,camdata,gendata)
    if(p==1 & r==1){
      #start a new table with headers on the first time through
      write.table(t(towrite), paste(outdir,"outputsummary.csv", sep=""), append=F, sep=",", row.names=F, col.names=T)
    }else{
      #add to table after first line
      write.table(t(towrite), paste(outdir,"outputsummary.csv", sep=""), append=T, sep=",", row.names=F, col.names=F)
    }
  } 
}

#generate some output and move directories so that do not overwrite output later

#create a folder with date and time so it is unique to new folder names
folder = gsub(" ", "", paste("../Output/output_", Sys.time(), ""), fixed = TRUE)
dir.create(folder)

#copy output into folder to use later
data = read.table("../Output/outputsummary.csv", header=T, sep="")
write.table(data, paste("../Output/", folder, "/outputsummary.csv", sep=""), row.names=F, col.names=T, sep=",")
