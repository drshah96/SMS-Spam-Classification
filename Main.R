
##### Session Information #####
sessionInfo()

##### Downloading File #####
#SMS Spam Data

if(!file.exists("Data/sms_spam.csv")) {
        download.file(url = "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/sms_spam.csv",
                      destfile = "Data/sms_spam.csv")
}

#Reading the Data File
data_raw <- read.csv("Data/sms_spam.csv", header = TRUE)

#Loading Source File
#source("Data Collection.R")

#Overview of Dataset
data <- data_raw
#Structure:
str(data)

#Dimension:
dim(data)

#Conversting text to character
data$text <- as.character(data$text)
str(data)

#
as.data.frame(head(data, n = 5))

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

#Corpus After Cleaning
inspect(data_corpus_clean[1:5])

#Creating sparse matrix from clean corpus
data_SparseMatrix <- DocumentTermMatrix(data_corpus_clean)
inspect(data_SparseMatrix)

#Testing and Training Datasets
data_train_raw <- data[1:4169, ]
data_test_raw <- data[4170:5559, ]

#Testing and Training Data Matrix
dataMatrix_train <- data_SparseMatrix[1:4169, ] 
dataMatrix_test <- data_SparseMatrix[4170:5559, ]

#Corpus Clean for Testing and Training Matrix
data_corpus_train <- data_corpus_clean[1:4169]
data_corpus_test <- data_corpus_clean[4170:5559]

#Performing Text Visualization Source File
#Loading Package
if(!require(wordcloud)) install.packages("wordcloud")
library(wordcloud)

#Word Cloud for Corpus Train Data
wordcloud(data_corpus_train, min.freq = 40, random.order = FALSE)

#Subsetting Train Dataset to extract Spam and Ham
spam <- subset(data_train_raw, type == "spam")
ham <- subset(data_train_raw, type == "ham")

#Word Cloud for Spam and Ham
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words =  40, scale = c(3, 0.5))


### Creating Indicator features for frequent words ###
FreqWords <- findFreqTerms(dataMatrix_train, 5)

#Saving List using Dictionary() Function
Dictionary <- function(x) {
        if( is.character(x) ) {
                return (x)
        }
        stop('x is not a character vector')
}

data_dict <- Dictionary(findFreqTerms(dataMatrix_train, 5))

#Appending Document Term Matrix to Train and Test Dataset 
data_train <- DocumentTermMatrix(data_corpus_train, list(data_dict))
data_test <- DocumentTermMatrix(data_corpus_test, list(data_dict))

#Converting the frequency of word to count
convert_counts <- function(x) {
        x <- ifelse(x > 0, 1, 0)
        x <- factor(x, levels = c(0, 1), labels = c("No", "Yes")) 
        return(x)
}

#Appending count function to Train and Test Dataset
data_train <- apply(data_train, MARGIN = 2, convert_counts)
data_test <- apply(data_test, MARGIN = 2, convert_counts)

#Naive Bayes
if(!require(e1071)) install.packages("e1071")
#Naive Bayes
library(e1071)
data_classifier <- naiveBayes(data_train, data_train_raw$type)

#### Evaluating Model Performance
data_test_pred <- predict(data_classifier, data_test)
library(gmodels)
CrossTable(data_test_pred, data_test_raw$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

##### Improving Model Performance 
data_classifier2 <- naiveBayes(data_train, data_train_raw$type,laplace = 1)

data_test_pred2 <- predict(data_classifier2, data_test)

CrossTable(data_test_pred2, data_test_raw$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
