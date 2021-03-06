#Code for assignment 1 part 1 for R programming

source('specDataRead.R', chdir = TRUE)

polluntantmean <- function(pollutant, fileInputs = 1:332) {
   
   if(dir.exists("specdata")){

       totalPollutant <- 0
       totalCount <- 0

       for(i in fileInputs){

           currentFrame <- na.omit(specDataRead(i)[pollutant])
           frameVector <- currentFrame[, pollutant]

           for ( j in frameVector) {
               totalPollutant <- j + totalPollutant
               totalCount <- 1 + totalCount
           }
       }

       totalPollutant / totalCount

   } else {
       print("ERROR: directory specdata not present")
   }
}