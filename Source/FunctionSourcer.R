#Set working directory and source functions
#setwd("~/GitHub/ABM-Course/Source")    # set temp working directory 
setwd("/Users/jannawilloughby/GDrive/Willoughby lab/furbearer abundance /noninvasive_modeling/Source/")

#source model functions
source(paste(getwd(), "/Landscape.R", sep = ''))
source(paste(getwd(), "/Pop.R", sep = ''))
source(paste(getwd(), "/Move.R", sep = ''))
source(paste(getwd(), "/Camera.R", sep = ''))
source(paste(getwd(), "/Genetics.R", sep = ''))
source(paste(getwd(), "/Tracking.R", sep = ''))
source(paste(getwd(), "/Collection.R", sep = ''))
