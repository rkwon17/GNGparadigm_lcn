#Emotion Project 7YF GNG Processing Script

###------HOW TO USE---------

#for use with GNG paradigm from Cragg & Nation (2008)
#created by RK and JT - please credit/cite if this script helps with analysis! 

####---potential packages that may be helpful
library(rprime)
library(stringr)
library(plyr)
library(dplyr)

#set directory and list relevant files 
setwd("/Volumes/dmc-nelson/Groups/DMC-Emotion-Project/Groups/Data/SevenYear/Behavioral/ProcessedData/GoNoGoScript/dropRAW")
#setwd("/home/jeb/Desktop/work/GNG/test")
filelist <- list.files(pattern = "gonogo7year")

#create a summary df that will be populated in the for loop
gng_summary <- data.frame(matrix(nrow = 0, ncol = 14))
colnames(gng_summary) <- c('subj','goHits','goMisses','goAnt','goOther','goHitRT','mixedHits', 'mixedMisses',
                           'mixedSI', 'mixedPI', 'mixedFI','mixedAnt','mixedOther', 'mixedHitRT')

for (i in 1:length(filelist)) {
  #create a new row in the summary df 
  gng_summary[i,] <- NA 
  #use the file name to extract the sub ID 
  subID <- str_match(filelist[i], "final-(.*?)-")[,2]
  #if necessary, add leading zero(s) to sub ID 
  if (nchar(subID) == 1) {
    subID = paste('00',subID,sep = "")
  } else if (nchar(subID) == 2) {
    subID = paste('0',subID,sep = "")
  } 
  #paste subject ID into summary df 
  gng_summary[i,'subj'] <- subID
  
  #loads data in appropriate format
  subPrime <- read_eprime(filelist[i])
  subData <- FrameList(subPrime)
  
  #go trials (2 blocks of 30 trials each)
  gotrials <- to_data_frame(filter_in(subData, 'Running', 'RTtriallist'))
  #if you get an error at this step, make sure that dplyr was loaded AFTER plyr
  gosummary <- count(gotrials, Response)
  
  if(any(gosummary$Response == 'HIT', na.rm = TRUE)) {
    gng_summary[i,'goHits'] <- gosummary[which(gosummary$Response == 'HIT'),'n']
  } else {
    gng_summary[i,'goHits'] <- 0
  }
  if(any(gosummary$Response == 'MISS', na.rm = TRUE)) {
    gng_summary[i,'goMisses'] <- gosummary[which(gosummary$Response == 'MISS'),'n']
  } else {
    gng_summary[i,'goMisses'] <- 0
  }
  if(any(gosummary$Response == 'A', na.rm = TRUE)) {
    gng_summary[i,'goAnt'] <- gosummary[which(gosummary$Response == 'A'),'n']
  } else {
    gng_summary[i,'goAnt'] <- 0
  }
  if(any(gosummary$Response == 'Other', na.rm = TRUE)) {
    gng_summary[i,'goOther'] <- gosummary[which(gosummary$Response == 'Other'),'n']
  } else {
    gng_summary[i,'goOther'] <- 0
  }
  
  #go trials rt
  gotrials$MouseClick2RT = as.numeric(as.character(gotrials$MouseClick2RT))
  gng_summary[i, 'goHitRT'] <- mean(subset(gotrials, Response == 'HIT')[,'MouseClick2RT'])
  
  #mixed trials (2 blocks of 52 each)
  mixedtrials <- to_data_frame(filter_in(subData, 'Running', 'triallist'))
  mixedsummary <- count(mixedtrials, Response)
  
  if(any(mixedsummary$Response == 'HIT', na.rm = TRUE)) {
    gng_summary[i,'mixedHits'] <- mixedsummary[which(mixedsummary$Response == 'HIT'),'n']
  } else {
    gng_summary[i,'mixedHits'] <- 0
  }
  if(any(mixedsummary$Response == 'MISS', na.rm = TRUE)) {
    gng_summary[i,'mixedMisses'] <- mixedsummary[which(mixedsummary$Response == 'MISS'),'n']
  } else {
    gng_summary[i,'mixedMisses'] <- 0
  }
  if(any(mixedsummary$Response == 'SI', na.rm = TRUE)) {
    gng_summary[i,'mixedSI'] <- mixedsummary[which(mixedsummary$Response == 'SI'),'n']
  } else {
    gng_summary[i,'mixedSI'] <- 0
  }
  if(any(mixedsummary$Response == 'PI', na.rm = TRUE)) {
    gng_summary[i,'mixedPI'] <- mixedsummary[which(mixedsummary$Response == 'PI'),'n']
  } else {
    gng_summary[i,'mixedPI'] <- 0
  }
  if(any(mixedsummary$Response == 'FI', na.rm = TRUE)) {
    gng_summary[i,'mixedFI'] <- mixedsummary[which(mixedsummary$Response == 'FI'),'n']
  } else {
    gng_summary[i,'mixedFI'] <- 0
  }
  if(any(mixedsummary$Response == 'A', na.rm = TRUE)) {
    gng_summary[i,'mixedAnt'] <- mixedsummary[which(mixedsummary$Response == 'A'),'n']
  } else {
    gng_summary[i,'mixedAnt'] <- 0
  }
  if(any(mixedsummary$Response == 'Other', na.rm = TRUE)) {
    gng_summary[i,'mixedOther'] <- mixedsummary[which(mixedsummary$Response == 'Other'),'n']
  } else {
    gng_summary[i,'mixedOther'] <- 0
  }
  
  #mixed trials rt
  mixedtrials$MouseClick2RT = as.numeric(as.character(mixedtrials$MouseClick2RT))
  gng_summary[i, 'mixedHitRT'] <- mean(subset(mixedtrials, Response == 'HIT')[,'MouseClick2RT'])
  
}

#accuracy/proportions
gng_summary[,2:ncol(gng_summary)] <- mutate_all(gng_summary[,2:ncol(gng_summary)], function(x) as.numeric(as.character(x)))

gng_summary$goHitProp <- gng_summary$goHits/(gng_summary$goHits+gng_summary$goMisses)
gng_summary$mixedCorrectProp <- (gng_summary$mixedHits+gng_summary$mixedSI)/
  (gng_summary$mixedHits+gng_summary$mixedMisses+gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedHitProp <- gng_summary$mixedHits/(gng_summary$mixedHits+gng_summary$mixedMisses)
gng_summary$mixedSIProp <- gng_summary$mixedSI/(gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedPIProp <- gng_summary$mixedPI/(gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedSIorPIProp <- (gng_summary$mixedSI+gng_summary$mixedPI)/
  (gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedCorrectPropPartial <- (gng_summary$mixedHits+gng_summary$mixedSI+gng_summary$mixedPI)/
  (gng_summary$mixedHits+gng_summary$mixedMisses+gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)

write.csv(gng_summary, 'gng_summary.csv')
