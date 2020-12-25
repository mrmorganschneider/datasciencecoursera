#Code for assignment 1 part 2 for R programming

source('specDataRead.R', chdir = TRUE)

complete <- function(fileInputs) {
   
   if(dir.exists("specdata")){

       outputFrame <- data.frame(matrix(ncol = 2, nrow = 0))
       

       for ( i in fileInputs){

           currentFrame <- specDataRead(i)

           nobsCount <- 0

           for ( j in 1:nrow(currentFrame)) {

               sulfateVal <- is.na(currentFrame[j,2])
               nitrateVal <- is.na(currentFrame[j,3])

               if(!sulfateVal & !nitrateVal){ nobsCount <- nobsCount + 1 }

           }

           outputFrame <- rbind(outputFrame, c(i,nobsCount))
       }
       
       colnames(outputFrame) <- c('id','nobs')
       outputFrame

   } else {
       print("ERROR: directory specdata not present")
   }
}