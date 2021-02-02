#Emotion Project 7YF GNG Processing Script

###------HOW TO USE---------

#created by RK and JT - please credit/cite if this script helps with analysis! 

####---potential libraries that may be helpful
library(rprime)
library(plyr)

setwd("/Volumes/dmc-nelson/Groups/DMC-Emotion-Project/Groups/Data/SevenYear/Behavioral/ProcessedData/GoNoGoScript/dropRAW")

#---- read in .txt file
subj07 <- read_eprime("gonogo7year_final-007-1.txt")
data07 <- FrameList(subj07)
preview_levels(data07)
preview_frames(data07)

not_practice <- filter_in(data07, "Running", "RTtriallist")
preview_levels(not_practice)
not_practice_df <- to_data_frame(not_practice)
str(not_practice_df)

#types of responses - HIT/MISS/A????? What is A?

