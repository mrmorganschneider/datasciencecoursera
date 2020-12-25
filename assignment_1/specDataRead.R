library(stringr, help, pos = 2, lib.loc = NULL)

specDataRead <- function(x) {
    fileName <- paste(str_pad(x,3,pad=0), ".csv", sep = "")
    read.csv(paste("specdata\\", fileName,  sep = ""))
}