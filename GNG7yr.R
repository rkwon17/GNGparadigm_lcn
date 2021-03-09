#Emotion Project 7YF GNG Processing Script

###------HOW TO USE---------

#created by RK and JT - please credit/cite if this script helps with analysis! 

####---potential libraries that may be helpful
library(rprime)
library(plyr)
library(dplyr)
setwd("/Volumes/dmc-nelson/Groups/DMC-Emotion-Project/Groups/Data/SevenYear/Behavioral/ProcessedData/GoNoGoScript/dropRAW")

#---- read in .txt file------------------------------
####-------s011 --------
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

###------s012
subj012 <-read_eprime("gonogo7year_final-012-1.txt")
data012 <- FrameList(subj012)
triallist_12 <- filter_in(data012, "Running", "triallist")
triallist12_df <- to_data_frame(triallist_12)

###------drop other values---------------------------------

#drop for go trials
gotrials_drop <-subset(gotrials, Response!="Other") #confirm w JT that we want to drop all rows with Other (not counted in final n)

#drop for GNG trials
triallist12drop_df<- subset(triallist12_df,Response!="Other")


####---------- summary and accuracy---------------------------

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

  
###---- reaction time variables ------------------------------------------
#figure out how to include into piping instead of hardcoding
gotrials$MouseClick1RT = as.numeric(as.character(gotrials$MouseClick1RT))
gotrials$MouseClick2RT = as.numeric(as.character(gotrials$MouseClick2RT))
gotrials$MouseClick3RT = as.numeric(as.character(gotrials$MouseClick3RT))

triallist11_df$MouseClick1RT = as.numeric(as.character(triallist11_df$MouseClick1RT))
triallist11_df$MouseClick2RT = as.numeric(as.character(triallist11_df$MouseClick2RT))
triallist11_df$MouseClick3RT = as.numeric(as.character(triallist11_df$MouseClick3RT))


s011GoTiming <- gotrials%>% filter(Response=="HIT") %>%
  mutate(MT=MouseClick2RT- MouseClick1RT) %>%
  select(TrialType,Response,ISI, RT = MouseClick1RT,MT,TotalDur=Target.RT)
  

#gng trials

s011GNGHIT<- triallist11_df %>% filter(Response=="HIT") %>%
  mutate(MT=MouseClick2RT- MouseClick1RT) %>%
  select(TrialType,Response,ISI, RT = MouseClick1RT,MT,TotalDur=Target.RT)

#PI 

s011GNG_PI<- triallist11_df %>% filter(Response=="PI") %>%
  mutate(MT=MouseClick2RT- MouseClick1RT) %>%
  select(TrialType,Response,ISI, RT = MouseClick1RT,MT,TotalDur=Target.RT)

#FI

s011GNG_FI<- triallist11_df %>% filter(Response=="FI") %>%
  mutate(MT=MouseClick2RT- MouseClick1RT) %>%
  select(TrialType,Response,ISI, RT = MouseClick1RT,MT,TotalDur=Target.RT)




#notes about ISI - randomized from 300 to 1100 ms regardless of block #
#block 1:

