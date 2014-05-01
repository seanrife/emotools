# Affect instability coefficient creation for ADAMA data
# Sean Chandler Rife, M.A.
# Kent State University
# Spring, 2014

# Clear output files, just in case...
unlink("affect_MSSD.csv")

# Chamber the round...
ADAMA_CHILD_DAILY <- read.csv(paste(getwd(),"/ADAMA_CHILD_DAILY.csv", sep=""))

# Require the psych library
library("psych")

# Now create some empty frames to hold everything
workingFrame <- data.frame(ADAMA_CHILD_DAILY$ID, ADAMA_CHILD_DAILY$panas_afraid, ADAMA_CHILD_DAILY$panas_angry)

finalFrame <- rmssd(workingFrame, group=workingFrame$ADAMA_CHILD_DAILY.ID, lag = 1)

write.csv(finalFrame,"affect_MSSD.csv", row.names=TRUE)