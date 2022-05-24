#setwd("~/GitHub/noninvasive_modeling/")
setwd("/Users/jannawilloughby/GDrive/Willoughby lab/furbearer abundance /noninvasive_modeling/Output/")

subfolder= "output_2022-05-2316:05:58"  #note that need to change folder names!

data = read.table(paste(subfolder, "/outputsummary.csv", sep=""), header=T, sep=",")
colnames(data) = c("rep","landscape","numindiv","numsteps","move","numcamera","camerror","numgen","generror","camrecO","camerrO","camuniqueO","genrecO","generrO","genuniqueO")

#identify variables/values for this set of data (may need to add more over time)
replicates = unique(data$rep)
numcamera  = unique(data$numcamera)
numgen     = unique(data$numgen)
camerror   = unique(data$camerror)
generror   = unique(data$generror)
numsteps   = unique(data$numsteps)
numindiv   = unique(data$numindiv)

OUT = NULL
for(c in 1:length(numcamera)){
  for(g in 1:length(numgen)){
    for(ce in 1:length(camerror)){
      for(ge in 1:length(generror)){
        for(ns in 1:length(numsteps)){
          for(ni in 1:length(numindiv)){
            t = data[data$numcamera==numcamera[c] & data$numgen==numgen[g] & data$camerror==camerror[ce] & data$generror==generror[ge] & data$numsteps==numsteps[ns] & data$numindiv==numindiv[ni],]
            
            #summarize camera output
            camercM = mean(t$camrecO)
            camercL = quantile(t$camrecO, probs=0.025)
            camercU = quantile(t$camrecO, probs=0.975)
            camerrM = mean(t$camerrO)
            camerrL = quantile(t$camerrO, probs=0.025)
            camerrU = quantile(t$camerrO, probs=0.975)
            camuniM = mean(t$camunique)
            camuniL = quantile(t$camunique, probs=0.025)
            camuniU = quantile(t$camunique, probs=0.975)
            
            #summarize genetic output
            genercM = mean(t$genrecO)
            genercL = quantile(t$genrecO, probs=0.025)
            genercU = quantile(t$genrecO, probs=0.975)
            generrM = mean(t$generrO)
            generrL = quantile(t$generrO, probs=0.025)
            generrU = quantile(t$generrO, probs=0.975)
            genuniM = mean(t$genunique)
            genuniL = quantile(t$genunique, probs=0.025)
            genuniU = quantile(t$genunique, probs=0.975)
            
            towrite = c(t$landscape[1], t$numindiv[1], t$numsteps[1], t$move[1], numcamera[c], numgen[g], camerror[ce], generror[ge], 
                        camercM, camercL, camercU, camerrM, camerrL, camerrU, camuniM, camuniL, camuniU,
                        genercM, genercL, genercU, generrM, generrL, generrU, genuniM, genuniL, genuniU)
            
            OUT = rbind(OUT, towrite)
          }
        }
      }
    }
  }
}
rownames(OUT) = 1:nrow(OUT)
colnames(OUT) = c("landscape", "numindiv", "numsteps", "move", "numcamera", "numgen", "camerror", "generror", 
                  "camercM", "camercL", "camercU", "camerrM", "camerrL", "camerrU", "camuniM", "camuniL", "camuniU",
                  "genercM", "genercL", "genercU", "generrM", "generrL", "generrU", "genuniM", "genuniL", "genuni")
write.table(OUT, (paste(subfolder, "/repsums.csv", sep="")), row.names=F, col.names=T, sep=",")

            