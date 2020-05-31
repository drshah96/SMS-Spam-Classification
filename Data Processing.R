##### SMS Spam Classification #####
### Data Cleaning and Processing ###

#Loading Source File
source("Data Collection.R")

#Overview of Dataset
data <- data_raw
str(data)

#Dimension of Dataset
dim(data)

#Conversting text to character
data$text <- as.character(data$text)
str(data)

#Checking for Null Values
any(is.na(data))

#Exploring Type column
class(data$type)

#Levels of Type Column
levels(data$type)

#Number of values in each column
table(data$type)

#Percentage of values
round(prop.table(table(data$type)), 2)

if(!require(tm)) install.packages("tm")
library(tm)

if(!require(NLP)) install.packages("NLP")
library(NLP)

#Creating Corpus
?Corpus
?VectorSource
data_corpus <- Corpus(VectorSource(data$text))
print(data_corpus)

#Inspecting Corpus
inspect(data_corpus[1:5])

### Cleaning Corpus ###
?tm_map
#Converting data to lower case
data_corpus_clean <- tm_map(data_corpus, tolower)

#Removing Numbers from the data
data_corpus_clean <- tm_map(data_corpus_clean, removeNumbers)

#Removing Filler Words
data_corpus_clean <- tm_map(data_corpus_clean, removeWords, stopwords())

#Removing Punctuation Markss
data_corpus_clean <- tm_map(data_corpus_clean, removePunctuation)

#Stripping Whitespace
data_corpus_clean <- tm_map(data_corpus_clean, stripWhitespace)

#Compaing Before Cleaning and After Cleaning Corpus
inspect(data_corpus[1:5])
inspect(data_corpus_clean[1:5])

#Creating sparse matrix from clean corpus
data_SparseMatrix <- DocumentTermMatrix(data_corpus_clean)
inspect(data_SparseMatrix)