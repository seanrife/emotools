# Affect differentiation coefficient creation for ADAMA data
# Sean Chandler Rife, M.A.
# Kent State University
# Spring, 2014

# Produces a CSV file that contains coefficients for affect differentiation
# Output can be merged with the original ADAMA dataset based on ID number

# ----------- FUNCTIONS -----------

# Function to get the average correlation based on a specified input matrix.
avgCorr <- function(inputMatrix) {

  # Pearson corr needs a nonzero SD to work with; if the SD is zero, r throws an error and returns NA (not helpful, since conceptually, this indicates very poor affect differentiation)
  # Check to make sure there's a nonzero SD; if there isn't, assign a value of 1

  # Remove NAs (sanity check)
  inputMatrix <- na.omit(inputMatrix)
  appendMe <- vector()
  
  if (is.na(sd(inputMatrix))) { 
    rAvg <- 1
  } 
  else if (sd(inputMatrix) == 0) {
    rAvg <- 1
  }
  # Sanity check: make sure there are no zero-SD variables lurking around. Loop through and check the SD of each variable to make sure it isn't zero (and if it is, nuke it).
  else if (0 %in% apply(inputMatrix, 2, sd)) {
    # Houston, we have a problem...
    rOutput <- cor(na.omit(inputMatrix), use="complete.obs", method="pearson")
    diag(rOutput) = NA
    addNum <- (sum(apply(inputMatrix, 2, sd) == 0))
    if (addNum > 1) {
      for (l in 1:(addNum*2)) {
        appendMe <- append(appendMe, 1)
      }
      rAvg <- mean((append(as.vector(rOutput), appendMe)), na.rm=TRUE)
    }
    else {
      rAvg <- mean((as.vector(rOutput)), na.rm=TRUE)
    }
    
  }
  else { 
    # Shiny! Run the correlation...
    rOutput <- cor(na.omit(inputMatrix), use="complete.obs", method="pearson")
    # Remove the diagonal (since it will be all 1s).
    diag(rOutput) = NA
    # ... and then get the mean of all the bivariate correlations.
    rAvg <- mean(rOutput, na.rm=TRUE)
  }
  return(rAvg)
}

# Function for Fisher's r-z transformation (allowing for +1/-1 input values)
fisherRZ <- function(inputr) {
  if (inputr == 1) {
    outputz <- 3
  }
  else if (inputr == -1) {
    outputz <- -3
  }
  else {
    outputz <- .5*(log((1+inputr)/(1-inputr)))
  }
  return(outputz)
}

# ----------- MAIN PROGRAM CODE -----------

# Chamber the round...
ADAMA_CHILD_DAILY <- read.csv(paste(getwd(),"/ADAMA_CHILD_DAILY.csv", sep=""))

# USED FOR TESTING
#ADAMA_CHILD_DAILY <- ADAMA_CHILD_DAILY[ADAMA_CHILD_DAILY[, "ID"] == 180,]

# Clear output files, just in case...
unlink("affectdiff.csv")

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
  rPOSavg <- avgCorr(workingMatrixPOS)
  rNEGavg <- avgCorr(workingMatrixNEG)
  
  # Wash everything through an r-z transformation
  # Sanity check first (make sure it's not passing a null value)
  if (!is.na(rPOSavg)) {
    rPOSz <- fisherRZ(round(rPOSavg, 8))
  }
  else {
    rPOSz <- NA
  }
  if (!is.na(rNEGavg)) {
    rNEGz <- fisherRZ(round(rNEGavg, 8))
  }
  else {
    rNEGz <- NA
  }
  
  # Reverse everything so it makes conceptual sense
  rPOSz <- rPOSz*-1
  rNEGz <- rNEGz*-1
  
  # Bind each to the existing frame
  outputFramePOS <- rbind(outputFramePOS, data.frame(ID = i, POSdiff = round(rPOSz, 8)))
  outputFrameNEG <- rbind(outputFrameNEG, data.frame(ID = i, NEGdiff = round(rNEGz, 8)))
  
  if (is.na(rPOSz)) POSnaCount <- POSnaCount+1
  if (is.na(rNEGz)) NEGnaCount <- NEGnaCount+1
  
}

# Done! Now add both output frames together by ID number
finalFrame <- merge(outputFramePOS, outputFrameNEG)
write.csv(finalFrame,"affectdiff.csv", row.names=FALSE)
#print(finalFrame)