rankall <- function(outcome, ranking = "best") {
     library(dplyr)

    #changes cases for inputs
    outcome <- tolower(outcome)

    #checks to make sure condition input is valid
    if (!(outcome == "heart attack"
        || outcome == "heart failure"
        || outcome == "pneumonia")){
            stop("Invalid Outcome")
        }

    hosData <- read.csv("outcome-of-care-measures.csv", 
    colClasses = "character")

    #pulls appropriate data based on the condition specified
    switch( outcome,
        "heart attack" = hosData <- hosData[,c(2,7,11)],
        "heart failure" = hosData <- hosData[,c(2,7,17)],
        "pneumonia" = hosData <- hosData[,c(2,7,23)]
    )

    #filters out Not Availables and sorts data
    names(hosData) <- c("Hospital", "State", "Rate")
    hosData <- filter(hosData, hosData[,3] != "Not Available")
    hosData$Rate <- as.numeric(hosData$Rate) 
    hosData <- hosData[order(hosData$State, hosData$Rate, hosData$Hospital),]

    hosData$Rank <- 1
    stateName <- "XX"
    rank <- 1

    for(i in 1:nrow(hosData)){
        if(hosData[i,2] != stateName){

            rank <- 1
            stateName <- hosData[i,2]
        }

        hosData[i,4] <- rank
        rank <- rank + 1

    }

    if(ranking == "best") {return(subset(hosData,Rank == 1))}

    else if(ranking == "worst"){

        hosData <- hosData[order(hosData$State, -hosData$Rate),]

        hosWorst <- data.frame(matrix(nrow = 0, ncol = 4))
        names(hosWorst) <- c("Hospital", "State", "Rate", "Rank")

        stateName <- "XX"
        for(i in 1:nrow(hosData)){
            if(hosData[i,2] != stateName){

                newFrame <- data.frame(
                    hosData[i,1], 
                    hosData[i,2],
                    hosData[i,3],
                    hosData[i,4])

                names(newFrame) <- c("Hospital", "State", "Rate", "Rank")
                hosWorst <- rbind(hosWorst,newFrame)

                stateName <- hosData[i,2]
            }
        }

        return(hosWorst)
    }
    
    else {return(subset(hosData, Rank == ranking))}
}