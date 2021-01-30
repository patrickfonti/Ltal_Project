


##### 0. Load packages ######
library(RCurl)
library(shiny)
library(shinythemes)
library(bslib)
library(thematic)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(readr)



##### 1. Functions ######

#' This function connect to the FTP of WSL and import the data from the 
#' import the data from the Ltal Campbell loggers
#' @param file is a vector of filenames to upload  
#' @param url is the ftp url
#' @param userpwd is the password of the ftp
#'
#' @return a tibble with the data in the file
#' @export
#'
#' @examples
import_FTP <- function(file, url, userpwd) {
  File <- getURL(paste0(url,'/',file), userpwd = userpwd, connecttimeout = 60) %>%
    gsub("\r\n","\n", .) %>%
    #read.table(text=., sep=",",  skip=1, header=TRUE) %>%
    read_delim(file=., delim=",",  skip=1, col_names = TRUE) %>%
    filter(row_number() != c(1,2)) %>% 
    write_delim(.,paste0(temp_directory,file), append = TRUE)
  return(File)
}

#' Title
#'
#' @param x is a vector of characters that need to be split
#'
#' @return
#' @export
#'
#' @examples
extract <- function(x) {unlist(strsplit((x),'_'))[[2]]}



##### 2. Import DATA #####

if(Sys.info()['nodename'] == "lema.wsl.ch") {
  temp_directory <- "/Volumes/Fonti/data/"
} else {
  if(Sys.info()['nodename'] %in% "shiny15") {
    temp_directory <- "/Volumes/Fonti/data/"
  }
}



# Connect to FTP
source('pw_FTP.R')
filenames <- unlist(strsplit(c(getURL(url, userpwd = userpwd,
  ftp.use.epsv = FALSE,dirlistonly = TRUE)),"[\n]"))

# Collect list of files names
filelist<- filenames[grep(pattern= "Table1", filenames)]
filelist

# Import data on Home/Fonti/data from files in the filelist
DATA <- map(filelist,import_FTP,url=url, userpwd = userpwd)
names(DATA) <- filelist

