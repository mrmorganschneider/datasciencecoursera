library(stringr, help, pos = 2, lib.loc = NULL)

specDataReadAll <- function(x) {
    read.csv(paste("specdata\\", x,  sep = ""))
}