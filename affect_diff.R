# Affect differentiation coefficient creation for longitudinal data
# Sean Chandler Rife, M.A.
# Kent State University
# Spring, 2014

# Returns a dataframe containing coefficients for affect differentiation

# ----------- FUNCTIONS -----------

# Function to get the average correlation based on a specified input matrix.
avgCorr <- function(inputMatrix) {
  
  # Pearson corr needs a nonzero SD to work with; if the SD is zero, r throws an error and returns NA (not helpful, since conceptually, this indicates very poor affect differentiation)
  # Check to make sure there's a nonzero SD; if there isn't, assign a value of 1
  
  # Remove NAs (sanity check)
  inputMatrix <- na.omit(inputMatrix)
  appendMe <- vector()
  
  if (is.na(sd(inputMatrix))) { 
    rAvg <- 3
  } 
  else if (sd(inputMatrix) == 0) {
    rAvg <- 3
  }
  # Sanity check: make sure there are no zero-SD variables lurking around. Loop through and check the SD of each variable to make sure it isn't zero (and if it is, nuke it).
  else if (0 %in% apply(inputMatrix, 2, sd)) {
    # Houston, we have a problem...
    rOutput <- cor(na.omit(inputMatrix), use="complete.obs", method="pearson")
    diag(rOutput) = NA
    addNum <- (sum(apply(inputMatrix, 2, sd) == 0))
    if (addNum > 1) {
      for (l in 1:(addNum*2)) {
        appendMe <- append(appendMe, 3)
      }
      outputz <- apply(rOutput, 1:2, function(x) fisherRZ(x))
      rAvg <- mean((append(as.vector(outputz), appendMe)), na.rm=TRUE)
    }
    else {
      outputz <- apply(rOutput, 1:2, function(x) fisherRZ(x))
      rAvg <- mean(outputz, na.rm=TRUE)
    }
    
  }
  else { 
    # Shiny! Run the correlation...
    rOutput <- cor(na.omit(inputMatrix), use="complete.obs", method="pearson")
    # Remove the diagonal (since it will be all 1s).
    diag(rOutput) = NA
    # ... and then get the mean of all the bivariate correlations, washed through a Fisher's R-Z transformation.
    outputz <- apply(rOutput, 1:2, function(x) fisherRZ(x))
    rAvg <- mean(outputz, na.rm=TRUE)
  }
  return(rAvg)
}

# Function for Fisher's r-z transformation (allowing for +1/-1 input values)
fisherRZ <- function(inputr) {
  if (is.na(inputr)) {
    outputz <- NA
  }
  else if (inputr >= .99) {
    outputz <- 3
  }
  else if (inputr <= -.99) {
    outputz <- -3
  }
  else {
    outputz <- .5*(log((1+inputr)/(1-inputr)))
  }
  return(outputz)
}

# ----------- MAIN PROGRAM FUNCTION -----------

affectdiff <- function(inFrame, IDvar, varList) {
  
  # Chamber the round...
  WORKING_DATA <- inFrame
  
  # Now create some empty frames to hold everything
  workingFrame <- data.frame()
  outputFrame <- data.frame(ID = numeric(0), diff = numeric(0))
  
  # Need to flag NAs and count them - create a variable to do that
  naCount <- 0
  
  # Create a vector of unique ID values that we can use to separate the data by ID and get one coefficient for each
  IDlist <- unique(WORKING_DATA$IDvar)
  
  # Loop through every ID number, select all the cases with that ID, and then awesome sauce
  for (i in IDlist){
    
    # Start by creating a data frame with only the selected ID number included
    workingFrame <- WORKING_DATA[WORKING_DATA[, idVar] == i,]
    
    # Now create matrices for each affect type, using the associated variables
    workingMatrix <- matrix(c(workingFrame$Q4, workingFrame$Q5, workingFrame$Q6), ncol=4)
    
    # Generate coefficients
    rAvg <- avgCorr(workingMatrix)
    
    # Reverse everything so it makes conceptual sense
    rz <- rAvg*-1
    
    # Bind each to the existing frame
    outputFrame <- rbind(outputFrame, data.frame(ID = i, diff = round(rz, 8)))
    
    # Count the number of missing cases - for testing
    if (is.na(rz)) naCount <- naCount+1
    
  }
  return(outputFrame)
  
}