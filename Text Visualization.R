##### SMS Spam Classification #####
### Visualizing Text Data ###

#Loading Source File
#source("Data Processing.R")

#Loading Package
if(!require(wordcloud)) install.packages("wordcloud")
library(wordcloud)

#Word Cloud for Corpus Train Data
wordcloud(data_corpus_train, min.freq = 40, random.order = FALSE)

#Subsetting Train Dataset to extract Spam and Ham
spam <- subset(data_train, type == "spam")
ham <- subset(data_train, type == "ham")

#Word Cloud for Spam and Ham
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words =  40, scale = c(3, 0.5))
