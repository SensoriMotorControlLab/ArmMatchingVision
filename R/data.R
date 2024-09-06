
summarizeCSV <- function(filename) {
  
  skiplines <- getLinesToSkip(filename)
  
  df <- read.csv(filename, skip=skiplines)
  
  df <- expandTrialNumbers(df)
  
  # select relevant rows
  
  # we want to go with all events for now:
  events <- c("STAY_CENTRE",
              "TARGET_ON",
              "HOLD_AT_TARGET",
              "WAIT_FOR_BUTTON",
              "TASK_BUTTON_10_CLICKED")
  
  # select only samples that have any of the events:
  df <- df[which(df$Event.name %in% events),]
  
  
  
  # select relevant columns:
  
  # let's say we want these columns
  # (might add some more later on)
  columns <- c( "Trial..",
                "Event.name",
                "Frame.time..s.",
                "Right..Hand.position.X",
                "Right..Hand.position.Y",
                "Left..Hand.position.X",
                "Left..Hand.position.Y")
  
  # so now we only take those columns from the data frame:
  df <- df[, columns]
  
  # we give these columns easier names to work with:
  names(df) <- c('trial_no', 'event_id', 'frametime_s', 'rightX_m', 'rightY_m', 'leftX_m', 'leftY_m')
  
  # and we convert them to numeric type:
  for (colname in c('frametime_s', 'rightX_m', 'rightY_m', 'leftX_m', 'leftY_m')) {
    colidx <- which(names(df) == colname)
    df[,colidx] <- as.numeric(df[,colidx])
  }
  
  # we return the much smaller data frame to the caller:
  return(df)  
  
}

getLinesToSkip <- function(filename) {
  
  # open a file connection with only reading rights
  # (we don't want to change the file accidentally)
  con <- file(filename,open="r")
  
  # we'll set this to FALSE when we find the start of the data
  ongoing <- TRUE
  
  # we need to keep track of which line we've previously read:
  lineno <- 0
  
  skip <- NA
  
  # as long as the start of the actual data hasn't been found,
  # we keep running this block of code:
  while (ongoing) { 
    
    # we read a single line:
    line <- readLines(con,           # from the file connection
                      n = 1)         # read 1 line
    
    # the length of the line will be 0 at the end of the file
    # so in that case, we should stop looking as well
    if (length(line) == 0) { ongoing <- FALSE }
    
    # the data part begins at a line that starts with "Trial #"
    if (substr(line,1,7) == "Trial #") {
      
      # we now know where to start reading the file:
      skip <- lineno
      
      # so if we see that we have found what we need
      # and can stop looking:
      ongoing <- FALSE
      
    }
    
    # we need to increment the line count:
    lineno <- lineno + 1
    
  }
  
  # close up the connection to the file:
  close(con)
  
  # return the relevant number:
  return(skip)
  
  # note that this will be NA if we didn't find a correct start
  
}

expandTrialNumbers <- function(df) {
  
  # it should start at trial 1,
  # but just in case we set it to 0
  current_trial <- 0
  
  # we take just the relevant column to work with:
  trialcol <- df$Trial..
  
  # we create a new vector to store continuous trial numbers:
  newtrialcol <- vector(mode='integer', length=length(trialcol))
  
  # we loop through the trial column entries:
  for (entry in c(1:length(trialcol))) {
    # if the entry has more than 0 characters, we check it out:
    if (nchar(trialcol[entry]) > 0) {
      # converting to numeric type should give NA (and a warning) if the string
      # contains something that ca not be converted to a numeric type:
      if (suppressWarnings(!is.na(as.numeric(trialcol[entry])))) {
        # if we do not get NA, we use this as the current trial number:
        current_trial <- as.numeric(trialcol[entry])
      }
    }
    # we store the current trial number in the new vector:
    newtrialcol[entry] <- current_trial
  }
  
  # and replace the trial# column with the new vector:
  df$Trial.. <- newtrialcol
  
  # and return the full data frame:
  return(df)
  
}