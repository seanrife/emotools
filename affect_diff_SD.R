# Affect differentiation coefficient creation for ADAMA data
# Sean Chandler Rife, M.A.
# Kent State University
# Spring, 2014

# Produces a CSV file that contains coefficients for affect differentiation
# Output can be merged with the original ADAMA dataset based on ID number

# ----------- FUNCTIONS -----------

# Function to get the average correlation based on a specified input matrix.
avgSD <- function(inputMatrix) {

  # Remove NAs (sanity check)
  inputMatrix <- na.omit(inputMatrix)
  workingSD <- apply(inputMatrix, 1, sd)
  sdAvg <- mean(workingSD)

  print(sdAvg)
  
  return(sdAvg)
}

# ----------- MAIN PROGRAM CODE -----------

# Chamber the round...
ADAMA_CHILD_DAILY <- read.csv(paste(getwd(),"/ADAMA_CHILD_DAILY.csv", sep=""))

# USED FOR TESTING
#ADAMA_CHILD_DAILY <- ADAMA_CHILD_DAILY[ADAMA_CHILD_DAILY[, "ID"] == 180,]

# Clear output files, just in case...
unlink("affectdiffSD.csv")

# Now create some empty frames to hold everything
workingFrame <- data.frame()
outputFramePOS <- data.frame(ID = numeric(0), POSdiff = numeric(0))
outputFrameNEG <- data.frame(ID = numeric(0), NEGdiff = numeric(0))
outputFrame <- data.frame()

# Need to flag NAs and count them - create a couple variables to do that
POSnaCount <- 0
NEGnaCount <- 0

# Create a vector of unique ID values that we can use to separate the data by ID and get one coefficient for each
IDlist <- unique(ADAMA_CHILD_DAILY$ID)

# Loop through every ID number, select all the cases with that ID, and then magic sauce
for (i in IDlist){
  
  # Start by creating a data frame with only the selected ID number included
  workingFrame <- ADAMA_CHILD_DAILY[ADAMA_CHILD_DAILY[, "ID"] == i,]
  
  # Now create matrices for each affect type, using the associated variables
  workingMatrixPOS <- matrix(c(workingFrame$panas_cheerful, workingFrame$panas_enthus, workingFrame$panas_happy), ncol=3)
  workingMatrixNEG <- matrix(c(workingFrame$panas_sad, workingFrame$panas_angry, workingFrame$panas_unhappy, workingFrame$panas_worried), ncol=4)
  
  # Generate coefficients for positive and negative affect diff
  rPOSavg <- avgSD(workingMatrixPOS)
  rNEGavg <- avgSD(workingMatrixNEG)
  
  # Wash everything through an r-z transformation
  # Sanity check first (make sure it's not passing a null value)
  
  
  # Bind each to the existing frame
  outputFramePOS <- rbind(outputFramePOS, data.frame(ID = i, POSdiff = round(rPOSavg, 8)))
  outputFrameNEG <- rbind(outputFrameNEG, data.frame(ID = i, NEGdiff = round(rNEGavg, 8)))
  
}

# Done! Now add both output frames together by ID number
finalFrame <- merge(outputFramePOS, outputFrameNEG)
write.csv(finalFrame,"affectdiffSD.csv", row.names=FALSE)
#print(finalFrame)