###
# This is to test connection to ftp of WSL
# this are the data from Matthias

library(tidyverse)

#### Functions #####

#' This function connect to the FTP of WSL and import the data from the 
#' import the data from the Ltal Campbell loggers
#' @param file  
#' @param url 
#' @param userpwd 
#'
#' @return a tibble with the data in the file
#' @export
#'
#' @examples
import_FTP <- function(file, url, userpwd) {
  File <- getURL(paste0(url,'/',file), userpwd = userpwd, connecttimeout = 60) %>%
    gsub("\r\n","\n", .) %>%
    read.table(text=., sep=",",  skip=1, header=TRUE) %>%
    filter(row_number() != c(1,2)) %>%
    as_tibble()
  return(File)
}


##### Import DATA #####

# Connect to FTP
source('pw_FTP.R')
filenames <- unlist(strsplit(c(getURL(url, userpwd = userpwd,
  ftp.use.epsv = FALSE,dirlistonly = TRUE)),"[\n]"))

# Collect list of files names
filelist<- filenames[grep(pattern= "Table1", filenames)]
filelist

# Import data from files in the filelist DATA
DATA <- map(filelist,import_FTP,url=url, userpwd = userpwd)
names(DATA) <- filelist




