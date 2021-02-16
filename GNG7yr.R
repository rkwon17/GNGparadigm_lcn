#Emotion Project 7YF GNG Processing Script

###------HOW TO USE---------

#created by RK and JT - please credit/cite if this script helps with analysis! 

####---potential libraries that may be helpful
library(rprime)
library(plyr)

setwd("/Volumes/dmc-nelson/Groups/DMC-Emotion-Project/Groups/Data/SevenYear/Behavioral/ProcessedData/GoNoGoScript/dropRAW")

#---- read in .txt file
#did s07 not come in ? 
subj07 <- read_eprime("gonogo7year_final-007-1.txt")
data07 <- FrameList(subj07)
preview_levels(data07)
preview_frames(data07)
#question - what is the difference between RT triallist and triallist
not_practice <- filter_in(data07, "Running", "RTtriallist")
preview_levels(not_practice)
not_practice_df <- to_data_frame(not_practice)
str(not_practice_df)

triallist <- filter_in(data07, "Running", "triallist")
preview_levels(triallist)
triallist_df <- to_data_frame(triallist)

#s011 
subj011<- read_eprime("gonogo7year_final-011-1.txt")
data011 <- FrameList(subj011)
preview_levels(data011)
preview_frames(data011)
#go no go trials
triallist_11 <- filter_in(data011, "Running", "triallist")
preview_levels(triallist_11)
triallist11_df <- to_data_frame(triallist_11)

#go trials only (?) 
gotrials <- filter_in(data011, "Running", "RTtriallist")
preview_levels(gotrials)
gotrials <- to_data_frame(gotrials)

gng_summary <- data.frame(matrix(nrow = 0, ncol = 12))
colnames(gng_summary) <- c('subj','goHits','goMisses','goAnt','goOther','mixedHits', 'mixedMisses',
                           'mixedSI', 'mixedPI', 'mixedFI','mixedAnt','mixedOther')

###within for loop
# for i in 1:length(subList) {
gng_summary[i,] <- NA

gng_summary[i,'subj'] <- '011' #hopefully can use something like subList[i] to read file and get subj ID
subjInd<- read_eprime("gonogo7year_final-011-1.txt") #replace with something iterative
dataInd <- FrameList(subjInd)

#go trials (2 blocks of 30 trials each)
gotrials <- to_data_frame(filter_in(dataInd, 'Running', 'RTtriallist'))
gosummary <- count(gotrials$Response)

if(any(gosummary$x == 'HIT')) {
  gng_summary[i,'goHits'] <- gosummary[which(gosummary$x == 'HIT'),'freq']
} else {
  gng_summary[i,'goHits'] <- 0
}
if(any(gosummary$x == 'MISS')) {
  gng_summary[i,'goMisses'] <- gosummary[which(gosummary$x == 'MISS'),'freq']
} else {
  gng_summary[i,'goMisses'] <- 0
}
if(any(gosummary$x == 'A')) {
  gng_summary[i,'goAnt'] <- gosummary[which(gosummary$x == 'A'),'freq']
} else {
  gng_summary[i,'goAnt'] <- 0
}
if(any(gosummary$x == 'Other')) {
  gng_summary[i,'goOther'] <- gosummary[which(gosummary$x == 'Other'),'freq']
} else {
  gng_summary[i,'goOther'] <- 0
}

#mixed trials (2 blocks of 52 each)
mixedtrials <- to_data_frame(filter_in(dataInd, 'Running', 'triallist'))
mixedsummary <- count(mixedtrials$Response)

if(any(mixedsummary$x == 'HIT')) {
  gng_summary[i,'mixedHits'] <- mixedsummary[which(mixedsummary$x == 'HIT'),'freq']
} else {
  gng_summary[i,'mixedHits'] <- 0
}
if(any(mixedsummary$x == 'MISS')) {
  gng_summary[i,'mixedMisses'] <- mixedsummary[which(mixedsummary$x == 'MISS'),'freq']
} else {
  gng_summary[i,'mixedMisses'] <- 0
}
if(any(mixedsummary$x == 'SI')) {
  gng_summary[i,'mixedSI'] <- mixedsummary[which(mixedsummary$x == 'SI'),'freq']
} else {
  gng_summary[i,'mixedSI'] <- 0
}
if(any(mixedsummary$x == 'PI')) {
  gng_summary[i,'mixedPI'] <- mixedsummary[which(mixedsummary$x == 'PI'),'freq']
} else {
  gng_summary[i,'mixedPI'] <- 0
}
if(any(mixedsummary$x == 'FI')) {
  gng_summary[i,'mixedFI'] <- mixedsummary[which(mixedsummary$x == 'FI'),'freq']
} else {
  gng_summary[i,'mixedFI'] <- 0
}
if(any(mixedsummary$x == 'A')) {
  gng_summary[i,'mixedAnt'] <- mixedsummary[which(mixedsummary$x == 'A'),'freq']
} else {
  gng_summary[i,'mixedAnt'] <- 0
}
if(any(mixedsummary$x == 'Other')) {
  gng_summary[i,'mixedOther'] <- mixedsummary[which(mixedsummary$x == 'Other'),'freq']
} else {
  gng_summary[i,'mixedOther'] <- 0
}

###end for loop here(?)

#accuracy/proportions
gng_summary[,2:12] <- mutate_all(gng_summary[,2:12], function(x) as.numeric(as.character(x)))

gng_summary$goHitProp <- gng_summary$goHits/(gng_summary$goHits+gng_summary$goMisses)

gng_summary$mixedHitProp <- gng_summary$mixedHits/(gng_summary$mixedHits+gng_summary$mixedMisses)
gng_summary$mixedSIProp <- gng_summary$mixedSI/(gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedCorrectProp <- (gng_summary$mixedHits+gng_summary$mixedSI)/
  (gng_summary$mixedHits+gng_summary$mixedMisses+gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedPIProp <- gng_summary$mixedPI/(gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedSIorPIProp <- (gng_summary$mixedSI+gng_summary$mixedPI)/
  (gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)
gng_summary$mixedCorrectPropPartial <- (gng_summary$mixedHits+gng_summary$mixedSI+gng_summary$mixedPI)/
  (gng_summary$mixedHits+gng_summary$mixedMisses+gng_summary$mixedSI+gng_summary$mixedPI+gng_summary$mixedFI)

  
#exclusionary criteria: 
# how many trials? 

