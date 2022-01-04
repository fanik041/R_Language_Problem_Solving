library(gsubfn)
library(stringr)
library(tidyverse)
library(lubridate)
x <- print(readLines('IrregularData-Excercise 2.3.txt', warn = FALSE))
T <- print(grepl("^//.*", x))

z <- print(x[T])

Tinv <- print(x[!T])

dateAsString <- print(str_extract(z, "21 May 2013")[[1]])

dateFinal <- print(as.Date(dateAsString, "%d %B %Y"))


excercise_2.3_d <- print(str_split(Tinv, ";"))
                    
dataRows <- print(lapply((excercise_2.3_d), function(x){x[!x ==""]}))

as.Date(str_extract(z[1], regex("[0-9]{2} [a-z]{3} [0-9]{4}", ignore_case = TRUE)), format = "%d %b %Y")

assignFields1 <- function(x){
  out <- character(3)
  # get 1st Column
  i <- grepl("[[:alpha:]]",x)
  out[1] <- x[i]
  # get 2nd Column (if any)
  i <- which(as.numeric(x) < 80)
  out[2] <- ifelse(length(i)>0, x[i], NA)
  # get 3rd Column (if any)
  i <- which(as.numeric(x) > 56)
  out[3] <- ifelse(length(i)>0, x[i], NA)
  out
}

assignFields <- function(x){
  out <- character(3)
  # get 1st Column
  i <- grepl("[[:alpha:]]",x)
  out[1] <- x[i]
  # get 2nd Column (if any)
  out[2] <- ifelse(x[2]!="", as.numeric(sub(",", ".", x[2], fixed = TRUE)), NA)
  # get 3rd Column (if any)
  out[3] <- ifelse(x[3]!="", as.numeric(sub(",", ".", x[3], fixed = TRUE)), NA)
  out
}

number_2.3_d_b <- print(lapply(excercise_2.3_d, assignFields))

(number_2.3_d_c <- matrix(unlist(number_2.3_d_b), nrow=length(number_2.3_d_b), byrow=TRUE))

splitData <- print(str_split(z, ":"))
(commentRows <- matrix(unlist(splitData), nrow=length(number_2.3_d_b), byrow=TRUE))
colHeadDraft <- print(data.frame(commentRows[, 2]))
colHead <- print(colHeadDraft[2:4, ])
colnames(number_2.3_d_c) <- c(colHead[1:3])
number_2.3_d_c




