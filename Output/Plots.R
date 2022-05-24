#create some output
library(scales)
setwd("/Users/jannawilloughby/GDrive/Willoughby lab/furbearer abundance /noninvasive_modeling/")

#call file in R (for questions 1-3)
data = read.table("Output/output_2022-04-2104:18:56/repsums.csv", header=T, sep=",") 

#find what variables we are working with
numcamera = sort(unique(data$numcamera))
numgen    = sort(unique(data$numgen))
camerror  = sort(unique(data$camerror))
generror  = sort(unique(data$generror))

#note: now interpreting camera as stationary and gen as moving sampling patterns

####question 1: is there a difference in number of detections across sampling efforts when stationary vs. mobile sampling patterns are used?####
#subset data
tdata = data[data$camerror==0.010 & data$generror==0.010 & data$numcamera==data$numgen, ] #filter to include only moderate error rates
colors2 = c("darkorchid3", "firebrick3")

#create plot outline
plot(-100,-100, xlim=c(0,max(c(data$numcamera,numgen))), ylim=c(0,2200), xlab="number sampling events", ylab="detections")

#add error bars for each group
for(r in 1:nrow(tdata)){
  segments(x0=tdata$numcamera[r], x1=tdata$numcamera[r], y0=tdata$camercL[r], y1=tdata$camercU[r], col=colors2[1])
  segments(x0=tdata$numcamera[r], x1=tdata$numcamera[r], y0=tdata$camercL[r], y1=tdata$camercU[r], col=colors2[1])
  segments(x0=tdata$numcamera[r]-2, x1=tdata$numcamera[r]+2, y0=tdata$camercL[r], y1=tdata$camercL[r], col=colors2[1])
  segments(x0=tdata$numcamera[r]-2, x1=tdata$numcamera[r]+2, y0=tdata$camercU[r], y1=tdata$camercU[r], col=colors2[1])
  
  segments(x0=tdata$numgen[r], x1=tdata$numgen[r], y0=tdata$genercL[r], y1=tdata$genercU[r], col=colors2[2])
  segments(x0=tdata$numgen[r], x1=tdata$numgen[r], y0=tdata$genercL[r], y1=tdata$genercU[r], col=colors2[2])
  segments(x0=tdata$numgen[r]-2, x1=tdata$numgen[r]+2, y0=tdata$genercL[r], y1=tdata$genercL[r], col=colors2[2])
  segments(x0=tdata$numgen[r]-2, x1=tdata$numgen[r]+2, y0=tdata$genercU[r], y1=tdata$genercU[r], col=colors2[2])
}
lines(x=tdata$numcamera, y=tdata$camercM, lwd=1.5, col=alpha(colors2[1], 0.8))
lines(x=tdata$numgen,    y=tdata$genercM, lwd=1.5, col=alpha(colors2[2], 0.8))
points(x=tdata$numcamera, y=tdata$camercM, pch=19, col=alpha(colors2[1], 0.5))
points(x=tdata$numgen,    y=tdata$genercM, pch=19, col=alpha(colors2[2], 0.5))

#answer1: both methods are not great at detection with very low effort, but even at moderate effort (25 events), 
#stationary samples detected more individuals 


####question 2: is there a difference in number of *unique* detections across sampling efforts when stationary vs. mobile sampling patterns are used?####
#subset data
tdata = data[data$camerror==0.010 & data$generror==0.010 & data$numcamera==data$numgen, ] #filter to include only moderate error rates
colors2 = c("darkorchid3", "firebrick3")

#create plot outline
plot(-100,-100, xlim=c(0,max(c(data$numcamera,numgen))), ylim=c(0,53), xlab="number sampling events", ylab="unique individuals detected")
abline(h=50, lty=2, col="grey50")

#add error bars for each group
for(r in 1:nrow(tdata)){
  segments(x0=tdata$numcamera[r], x1=tdata$numcamera[r], y0=tdata$camuniL[r], y1=tdata$camuniU[r], col=colors2[1])
  segments(x0=tdata$numcamera[r], x1=tdata$numcamera[r], y0=tdata$camuniL[r], y1=tdata$camuniU[r], col=colors2[1])
  segments(x0=tdata$numcamera[r]-2, x1=tdata$numcamera[r]+2, y0=tdata$camuniL[r], y1=tdata$camuniL[r], col=colors2[1])
  segments(x0=tdata$numcamera[r]-2, x1=tdata$numcamera[r]+2, y0=tdata$camuniU[r], y1=tdata$camuniU[r], col=colors2[1])
  
  segments(x0=tdata$numgen[r], x1=tdata$numgen[r], y0=tdata$genuniL[r], y1=tdata$genuni[r], col=colors2[2])
  segments(x0=tdata$numgen[r], x1=tdata$numgen[r], y0=tdata$genuniL[r], y1=tdata$genuni[r], col=colors2[2])
  segments(x0=tdata$numgen[r]-2, x1=tdata$numgen[r]+2, y0=tdata$genuniL[r], y1=tdata$genuniL[r], col=colors2[2])
  segments(x0=tdata$numgen[r]-2, x1=tdata$numgen[r]+2, y0=tdata$genuni[r], y1=tdata$genuni[r], col=colors2[2])
}
lines(x=tdata$numcamera, y=tdata$camuniM, lwd=1.5, col=alpha(colors2[1], 0.8))
lines(x=tdata$numgen,    y=tdata$genuniM, lwd=1.5, col=alpha(colors2[2], 0.8))
points(x=tdata$numcamera, y=tdata$camuniM, pch=19, col=alpha(colors2[1], 0.5))
points(x=tdata$numgen,    y=tdata$genuniM, pch=19, col=alpha(colors2[2], 0.5))

#answer2: both approaches detect the same number of unique individuals (95%CIs overlap) 

####question 3: does identification error rate alter the similar effectiveness of stationary and moving sampling approaches?####
tdata = data[data$camerror==data$generror & data$numcamera==data$numgen, ] #filter to include only moderate error rates
colors2   = c("darkorchid3", "firebrick3")
linetypes = c(1,2,3)

#create plot outline
plot(-100,-100, xlim=c(0,max(c(data$numcamera,numgen))), ylim=c(0,53), xlab="number sampling events", ylab="unique individuals detected")
abline(h=50, lty=2, col="grey50")

#add error bars for each group
for(e in 1:length(camerror) ){
  ttdata = tdata[tdata$camerror==camerror[e],]
  for(r in 1:nrow(ttdata)){
    segments(x0=ttdata$numcamera[r], x1=ttdata$numcamera[r], y0=ttdata$camuniL[r], y1=ttdata$camuniU[r], col=colors2[1])
    segments(x0=ttdata$numcamera[r], x1=ttdata$numcamera[r], y0=ttdata$camuniL[r], y1=ttdata$camuniU[r], col=colors2[1])
    segments(x0=ttdata$numcamera[r]-2, x1=ttdata$numcamera[r]+2, y0=ttdata$camuniL[r], y1=ttdata$camuniL[r], col=colors2[1])
    segments(x0=ttdata$numcamera[r]-2, x1=ttdata$numcamera[r]+2, y0=ttdata$camuniU[r], y1=ttdata$camuniU[r], col=colors2[1])
    
    segments(x0=ttdata$numgen[r], x1=ttdata$numgen[r], y0=ttdata$genuniL[r], y1=ttdata$genuni[r], col=colors2[2])
    segments(x0=ttdata$numgen[r], x1=ttdata$numgen[r], y0=ttdata$genuniL[r], y1=ttdata$genuni[r], col=colors2[2])
    segments(x0=ttdata$numgen[r]-2, x1=ttdata$numgen[r]+2, y0=ttdata$genuniL[r], y1=ttdata$genuniL[r], col=colors2[2])
    segments(x0=ttdata$numgen[r]-2, x1=ttdata$numgen[r]+2, y0=ttdata$genuni[r], y1=ttdata$genuni[r], col=colors2[2])
  }
  lines(x=ttdata$numcamera, y=ttdata$camuniM, lwd=1.5, col=alpha(colors2[1], 0.8), lty=linetypes[e])
  lines(x=ttdata$numgen,    y=ttdata$genuniM, lwd=1.5, col=alpha(colors2[2], 0.8), lty=linetypes[e])
  points(x=ttdata$numcamera, y=ttdata$camuniM, pch=19, col=alpha(colors2[1], 0.5))
  points(x=ttdata$numgen,    y=ttdata$genuniM, pch=19, col=alpha(colors2[2], 0.5))
}

#answer3: error rate does not seem to effect this relationship

####question 4: does movement capacity of target/vector organism make either stationary or moving sampling approaches more effective?
#call file in R (for questions 4-5)
data = read.table("Output/output_2022-05-2315:21:09/repsums.csv", header=T, sep=",")

tdata = data
colors2   = c("darkorchid3", "firebrick3")
linetypes = c(1,2,3)

#create plot outline
plot(-100,-100, xlim=c(0,max(c(data$numcamera,numgen))), ylim=c(0,53), xlab="number sampling events", ylab="unique individuals detected")
abline(h=50, lty=2, col="grey50")

#add error bars for each group
for(e in 1:length(camerror) ){
  ttdata = tdata[tdata$camerror==camerror[e],]
  for(r in 1:nrow(ttdata)){
    segments(x0=ttdata$numcamera[r], x1=ttdata$numcamera[r], y0=ttdata$camuniL[r], y1=ttdata$camuniU[r], col=colors2[1])
    segments(x0=ttdata$numcamera[r], x1=ttdata$numcamera[r], y0=ttdata$camuniL[r], y1=ttdata$camuniU[r], col=colors2[1])
    segments(x0=ttdata$numcamera[r]-2, x1=ttdata$numcamera[r]+2, y0=ttdata$camuniL[r], y1=ttdata$camuniL[r], col=colors2[1])
    segments(x0=ttdata$numcamera[r]-2, x1=ttdata$numcamera[r]+2, y0=ttdata$camuniU[r], y1=ttdata$camuniU[r], col=colors2[1])
    
    segments(x0=ttdata$numgen[r], x1=ttdata$numgen[r], y0=ttdata$genuniL[r], y1=ttdata$genuni[r], col=colors2[2])
    segments(x0=ttdata$numgen[r], x1=ttdata$numgen[r], y0=ttdata$genuniL[r], y1=ttdata$genuni[r], col=colors2[2])
    segments(x0=ttdata$numgen[r]-2, x1=ttdata$numgen[r]+2, y0=ttdata$genuniL[r], y1=ttdata$genuniL[r], col=colors2[2])
    segments(x0=ttdata$numgen[r]-2, x1=ttdata$numgen[r]+2, y0=ttdata$genuni[r], y1=ttdata$genuni[r], col=colors2[2])
  }
  lines(x=ttdata$numcamera, y=ttdata$camuniM, lwd=1.5, col=alpha(colors2[1], 0.8), lty=linetypes[e])
  lines(x=ttdata$numgen,    y=ttdata$genuniM, lwd=1.5, col=alpha(colors2[2], 0.8), lty=linetypes[e])
  points(x=ttdata$numcamera, y=ttdata$camuniM, pch=19, col=alpha(colors2[1], 0.5))
  points(x=ttdata$numgen,    y=ttdata$genuniM, pch=19, col=alpha(colors2[2], 0.5))
}
