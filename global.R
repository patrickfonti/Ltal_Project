


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
library(tibble)



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
  if(Sys.info()['nodename'] == "lema.wsl.ch" | Sys.info()['nodename'] == "lema.local") {
    temp_directory <- '/Volumes/Fonti/data/'
  } else {
    if(Sys.info()['nodename'] %in% "shiny15") {
      temp_directory <- '/home/fonti/data/ltal/'
    }
  }
  
  File <- getURL(paste0(url,'/',file), userpwd = userpwd, connecttimeout = 60) %>%
    gsub("\r\n","\n", .) %>%
    read_delim(file=., delim=",",  skip=1, col_names = TRUE) %>%
    filter(row_number() != c(1,2)) %>% 
    write_csv(x=. ,path=paste0(temp_directory,file), col_names = TRUE, append=FALSE) 
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

# Connect to FTP
source('pw_FTP.R')
filenames <- unlist(strsplit(c(getURL(url, userpwd = userpwd,
                                      ftp.use.epsv = FALSE,dirlistonly = TRUE)),"[\n]"))

# Collect list of files names
filelist<- filenames[grep(pattern= "Table1", filenames)]
filelist

# Import data on home/fonti/data from files in the filelist
upload<- function(x) {read_delim(x, delim=",", col_names = TRUE)}

if(Sys.info()['nodename'] == "lema.wsl.ch" | Sys.info()['nodename'] == "lema.local") {
  DATA <- map(filelist,import_FTP,url=url, userpwd = userpwd)
  names(DATA) <- filelist
} else {
  if(Sys.info()['nodename'] %in% "shiny15") {
    DATA <- map(filelist,import_FTP,url=url, userpwd = userpwd)
    DATA <- map(paste0(temp_directory,filelist), upload)
    names(DATA) <- filelist
  }
}
