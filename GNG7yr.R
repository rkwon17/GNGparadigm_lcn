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

#correct/ incorrect for go trials only
  #factor HIT, MISS, Other
gotrials$responsenum <- ifelse( gotrials$Response== "HIT", 1, ifelse(gotrials$Response == "MISS", 0, 9))
gotrials %>% count(responsenum)

  
  
#exclusionary criteria: 
# how many trials? 

