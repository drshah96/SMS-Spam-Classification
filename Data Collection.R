##### SMS Spam Classification #####

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
