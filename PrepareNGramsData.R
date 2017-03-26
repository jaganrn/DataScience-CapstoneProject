# Prepare N-Grams Data
#
# This program is to generate the N-Gram data for the Coursera Data Science Capstone Project
#
# 1. Download the data from the HC Corpora
# 2. Prepare the traning dataset (as the whole data is too huge). Also combines all sources
# 3. Normalize the sentenses
# 4. Tokenize the input dataset to N-Grams (1, 2, 3, 4, 5 grams)
# 5. Create Term Document Frequency Matrix
# 6. Create the data.table representing the N-Grams
#
# Written by Jagannatha Reddy
#

#load the required libraries
library(tm)
library(stringr)
library(RWeka)
library(data.table)

#download the data if not present already
cache=TRUE
workDir <- "/Users/jagan/work/DataScience/DataScienceCapstone/DataScience-CapstoneProject"
setwd(workDir)
destFile <- "Coursera-SwiftKey.zip"
if (!file.exists(destFile)) {
    fileUrl <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(fileUrl, destfile=destFile)
}
blogsFile <- "./final/en_US/en_US.blogs.txt"
newsFile <- "./final/en_US/en_US.news.txt"
twitterFile <- "./final/en_US/en_US.twitter.txt"

#unzip only if any of the input files not present
if(!file.exists(blogsFile) || !file.exists(newsFile) || !file.exists(twitterFile)) {
    unzip(destFile)
}

#read the files
blogs <- readLines(blogsFile, encoding="UTF-8")
news  <- readLines(newsFile, encoding="UTF-8")
twitter <- readLines(twitterFile, encoding="UTF-8")

#compute the line count
blogsLineCount   <- length(blogs)
newsLineCount    <- length(news)
twitterLineCount <- length(twitter)

#define the number of lines required for training set. Analysis done with various sizes
trainingSetLines <- 20000
trainingBlogs   <- blogs[sample(1:blogsLineCount, trainingSetLines)]
trainingNews    <- news[sample(1:newsLineCount, trainingSetLines)]
trainingTwitter <- twitter[sample(1:twitterLineCount, trainingSetLines)]

#create data directory
dir.create("data", showWarnings=FALSE) #ignore Warning to recreate the directory

#create a combined training dataset
trainingDataFile <- "data/en_US-Training.txt"
fHandle <- file(trainingDataFile, "wt")
writeLines(trainingBlogs, con=fHandle)
writeLines(trainingNews, con=fHandle)
writeLines(trainingTwitter, con=fHandle)
close(fHandle)

#load the training data and perform basic cleaning to normalize the data
trainingCorpus <- Corpus(DirSource(paste(workDir, "/data", sep=""))) #create the corpus from sample data
trainingCorpus <- tm_map(trainingCorpus, content_transformer(tolower)) #convert to lowercase
trainingCorpus <- tm_map(trainingCorpus, removePunctuation) #remove punctuations
trainingCorpus <- tm_map(trainingCorpus, stripWhitespace) #eliminate extra whitespaces

#define the tokenizer functions 
tokenizer1Word <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
tokenizer2Word <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
tokenizer3Word <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
tokenizer4Word <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
tokenizer5Word <- function(x) NGramTokenizer(x, Weka_control(min=5, max=5))

#create the term document frequency matrix
tdm1 <- TermDocumentMatrix(trainingCorpus, control=list(tokenize=tokenizer1Word))
tdm2 <- TermDocumentMatrix(trainingCorpus, control=list(tokenize=tokenizer2Word))
tdm3 <- TermDocumentMatrix(trainingCorpus, control=list(tokenize=tokenizer3Word))
tdm4 <- TermDocumentMatrix(trainingCorpus, control=list(tokenize=tokenizer4Word))
tdm5 <- TermDocumentMatrix(trainingCorpus, control=list(tokenize=tokenizer5Word))

#sort the data based on the frequency
freq1 <- sort(rowSums(as.matrix(tdm1)), decreasing=TRUE)
freq2 <- sort(rowSums(as.matrix(tdm2)), decreasing=TRUE)
freq3 <- sort(rowSums(as.matrix(tdm3)), decreasing=TRUE)
freq4 <- sort(rowSums(as.matrix(tdm4)), decreasing=TRUE)
freq5 <- sort(rowSums(as.matrix(tdm5)), decreasing=TRUE)

#create data tables of word frequencies for easy lookups
wf1 <- data.table(terms=names(freq1), freq=freq1)
wf2 <- data.table(terms=names(freq2), freq=freq2)
wf3 <- data.table(terms=names(freq3), freq=freq3)
wf4 <- data.table(terms=names(freq4), freq=freq4)
wf5 <- data.table(terms=names(freq5), freq=freq5)

#consider the word combinations only above certain threshold
THRESHOLD <- 1
TFT1 <- wf1[wf1$freq > THRESHOLD,]
TFT2 <- wf2[wf2$freq > THRESHOLD,]
TFT3 <- wf3[wf3$freq > THRESHOLD,]
TFT4 <- wf4[wf4$freq > THRESHOLD,]
TFT5 <- wf5[wf5$freq > THRESHOLD,]
