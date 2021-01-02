best <- function(inpState, outcome) {

    library(dplyr)

    #changes cases for inputs
    inpState <- toupper(inpState)
    outcome <- tolower(outcome)

    #checks to make sure condition input is valid
    if (!(outcome == "heart attack"
        || outcome == "heart failure"
        || outcome == "pneumonia")){
            stop("Invalid Outcome")
        }

    hosData <- read.csv("outcome-of-care-measures.csv", 
    colClasses = "character")

    #checks for valid state
    stateData <- subset(hosData, State==inpState)
    if (nrow(stateData) == 0) {
       stop("Invalid State")
    }

    #pulls appropriate data based on the condition specified
    switch( outcome,
        "heart attack" = stateData <- stateData[,c(2,11)],
        "heart failure" = stateData <- stateData[,c(2,17)],
        "pneumonia" = stateData <- stateData[,c(2,23)]
    )

    #filters out Not Availables and sorts data
    names(stateData) <- c("Hospital", "Condition")
    stateData <- filter(stateData, stateData[,2] != "Not Available")

    stateData$Condition <- as.numeric(stateData$Condition) 

    stateData <- stateData[order(stateData$Condition),]

    outputFrame <- stateData[1,1]
        
    if(stateData[2,2] == stateData[1,2]){
        outputFrame <- rbind(outputFrame, stateData[2,1])
    }
    
    outputFrame
    #head(stateData)

}