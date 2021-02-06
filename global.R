


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
  File <- getURL(paste0(url,'/',file), userpwd = userpwd, connecttimeout = 60) %>%
    gsub("\r\n","\n", .) %>%
    read_delim(file=., delim=",",  skip=1, col_names = TRUE) %>%
    filter(row_number() != c(1,2)) 
  return(File)
}

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
import_FTP1 <- function(file, url, userpwd) {
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
extract1 <- function(x) {unlist(strsplit((x),'_'))[[1]]}

#' Title
#'
#' @param x is a vector of characters that need to be split
#'
#' @return
#' @export
#'
#' @examples
extract2 <- function(x) {unlist(strsplit((x),'_'))[[2]]}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
upload<- function(x) {read_delim(x, delim=",", col_names = TRUE)}


##### 2. Import data for setting plot in Home
Setting <- read_csv("data/Setting.csv")


##### 3. Import DATA Campbell #####

# define working directory to write files
if(Sys.info()['nodename'] %in% "shiny15") {
  if (!dir.exists('/home/fonti/data/ltal')) {dir.create('/home/fonti/data/ltal')}
    temp_directory <- '/home/fonti/data/ltal/'
  }


# Connect to FTP
source('pw_FTP.R')
filenames <- unlist(strsplit(c(getURL(url, userpwd = userpwd,
                                      ftp.use.epsv = FALSE,dirlistonly = TRUE)),"[\n]"))

# Collect list of files names
filelist<- filenames[grep(pattern= "Table1", filenames)]
filelist

# Import data on home/fonti/data from files in the filelist

if(Sys.info()['nodename'] == "lema.wsl.ch" | Sys.info()['nodename'] == "lema.local") {
  DATA <- map(filelist,import_FTP,url=url, userpwd = userpwd)
  names(DATA) <- filelist
} else {
  if(Sys.info()['nodename'] %in% "shiny15") {
    DATA <- map(filelist,import_FTP1,url=url, userpwd = userpwd)
    DATA <- map(paste0(temp_directory,filelist), upload)
    names(DATA) <- filelist
  }
}


##### 4. Import DATA Deendrometers #####

# Connect to Ltal clean Folder
DENDRO<-read_delim("~/Desktop/Ltal/Ltal DATA/Dendro/data/2. Clean/all.DENDRO_havg.txt", delim="\t", col_names=TRUE, col_types = cols(.default = "d", Index = "T"))

# Import data on home/fonti/data from files in the filelist
upload<- function(x) {read_delim(x, delim=",", col_names = TRUE)}

if(Sys.info()['nodename'] == "lema.wsl.ch" | Sys.info()['nodename'] == "lema.local") {
  write_csv(x=DENDRO ,path=paste0(getwd(),"/data/DENDRO.csv"), col_names = TRUE) 
} else {
  write_csv(x=DENDRO ,path='/home/fonti/data/ltal/DENDRO.csv', col_names = TRUE) 
}



