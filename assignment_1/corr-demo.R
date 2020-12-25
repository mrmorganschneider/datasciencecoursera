#Code for assignment 1 part 2 for R programming

source('specDataReadAll.R', chdir = TRUE)

corr <- function(minimum=0) {
   
   if(dir.exists("specdata")){       

       outputVector <- vector()
       for ( i in list.files("specdata")){

           currentFrame <- specDataReadAll(i)
           nobsCount <- 0

           for ( j in 1:nrow(currentFrame)) {

               sulfateVal <- is.na(currentFrame[j,2])
               nitrateVal <- is.na(currentFrame[j,3])

               if(!sulfateVal & !nitrateVal){ nobsCount <- nobsCount + 1 }

           }

           if (nobsCount >= minimum){
               corrDataFrame <- currentFrame[, c('sulfate','nitrate')]
               #print(cor(corrDataFrame,use="complete.obs"))
               outputVector <- c(outputVector,cor(corrDataFrame,use="na.or.complete")[1,2])
           }
       }

       outputVector
   }
}