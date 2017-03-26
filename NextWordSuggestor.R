# Coursera Capstone Project
#
# This program suggests the next word given the set of words. Assumes the data is already loaded by running NextWordSuggestor.R
#
# Written by Jagannatha Reddy
#

#include required packages
library(stringr)
library(data.table)

#function to normalize the given input string for lookup
NormalizeString <- function(inputString) {
    inputCorpus <- VCorpus(VectorSource(inputString)) #convert the string to corpus to operate further
    inputCorpus <- tm_map(inputCorpus, content_transformer(tolower)) #convert to lowercase
    inputCorpus <- tm_map(inputCorpus, stripWhitespace) #eliminate extra whitespaces
    inputString <- as.character(inputCorpus[[1]])
    inputString <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", inputString) #remove extra white spaces at both ends
    inputString
}

#given an input string, suggest the next word
SuggestNextWord <- function(givenString) {
    givenString <- NormalizeString(givenString)  #normalize the input using the same mechanism as that of training data
  
    givenStringList <- unlist(strsplit(givenString, split=" ")) #convert the input to vector form
    givenStringLen  <- length(givenStringList)
  
    suggestedTerm  <- ""
    foundResult <- FALSE
  
    #try to match using 5-gram when given string has more than 3 words
    if(givenStringLen >= 4 & !foundResult) {
        #try to match the last 4 words in a given string
        currentMatchString <- paste(givenStringList[(givenStringLen-3):givenStringLen], collapse=" ")
        #check for presence in 5-gram data
        TFT5Temp <- TFT5[grep(paste("^", currentMatchString, sep = ""), TFT5$terms), ]
    
        #Check to see if any matching record returned
        if(length(TFT5Temp[, 1]) > 0) {
            suggestedTerm <- TFT5Temp[1, 1]
            foundResult <- TRUE
        }
        TFT5Temp <- NULL
    }
  
    #try to match using 4-gram if result is not found yet and given string has more than 2 words
    if(givenStringLen >= 3 & !foundResult) {
        #try to match the last 3 words in a given string
        currentMatchString <- paste(givenStringList[(givenStringLen-2):givenStringLen], collapse=" ")
        #check for presence in 4-gram data
        TFT4Temp <- TFT4[grep(paste("^",currentMatchString, sep = ""), TFT4$terms), ]
    
        #Check to see if any matching record returned
        if(length(TFT4Temp[, 1]) > 0) {
            suggestedTerm <- TFT4Temp[1, 1]
            foundResult <- TRUE
        }
        TFT4Temp <- NULL
    }
  
    #try to match using 3-gram if result is not found yet and given string has more than 1 word
    if(givenStringLen >= 2 & !foundResult) {
        #try to match the last 2 words in a given string
        currentMatchString <- paste(givenStringList[(givenStringLen-1):givenStringLen], collapse=" ")
    
        #check for presence in 3-gram data
        TFT3Temp <- TFT3[grep(paste("^",currentMatchString, sep = ""), TFT3$terms), ]
    
        #Check to see if any matching record returned
        if(length(TFT3Temp[, 1]) > 0) {
            suggestedTerm <- TFT3Temp[1,1]
            foundResult <- TRUE
        }
        TFT3Temp <- NULL
    }
  
    #finally try to match using 2-gram if result is not found yet and using the last word
    if(givenStringLen >= 1 & !foundResult) {
        #try to match the last word in a given string
        currentMatchString <- givenStringList[givenStringLen]
    
        #check for presence in 4-gram data
        TFT2Temp <- TFT2[grep(paste("^",currentMatchString, sep = ""), TFT2$terms), ]
    
        #Check to see if any matching record returned
        if(length(TFT2Temp[, 1]) > 0) {
            suggestedTerm <- TFT2Temp[1,1]
            foundResult <- TRUE
        }
        TFT2Temp <- NULL
    }
  
    if(foundResult) {
        return(paste(givenString, word(suggestedTerm, -1)))
    } else {
        if(givenString == "") {
            return("Enter one or more words into the Input Text Box at the left panel")
        } else {
            return("NO_SUGGESTION")
        }
    }
}
