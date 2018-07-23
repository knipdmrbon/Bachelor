#############################################################################################
### Authors: Dominik Maruszczak & Fabian Pribahsnik                                       ###
### Purpose: Bachelor Thesis                                                              ###
#############################################################################################


#############################################################################################
############################### Variable description ########################################
#############################################################################################
# DT.header --------------------> Header for the raw-data except last column (is spam or not)
# DT.rawData -------------------> Raw data, read directly from the data-repository. (http://archive.ics.uci.edu/ml/datasets/Spambase?ref=datanews.io)
# DT.train ---------------------> 90% of the raw-data used for training the different models. 
# DT.test ----------------------> 10% of the raw-data used for testing the accuracy of the different models. 
# 
#
#
#
#############################################################################################



############## load all required packages ##############
library(rstudioapi) # for setting the working directory to the path of the script
library(data.table) 
library(caret)
library(ggplot2)
library(corrplot)
library(viridis) # new colors

############## Read and prepare the raw-data ##############
set.seed(100) # set the seed to get similar results for the random-parts
# set the working directory to the path of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# read the header of the data
DT.header <- fread("spambase/header.txt", header = FALSE)
# read the raw-data
DT.rawData <- fread("spambase/spambase.data", sep = ",", header = FALSE)
# create header for the data
names(DT.rawData) <- c(as.vector(DT.header$V1), "is_spam_flag")
# define the flag for spam as factor
DT.rawData$is_spam_flag <- factor(DT.rawData$is_spam_flag, labels = c("No", "Yes"))

# Find randomly selected rownumbers of the raw data so that we get 90% of data for training
train_rows <- createDataPartition(DT.rawData$is_spam_flag, p = 0.9, list = FALSE)
# create data.table for training purposes
DT.train <- DT.rawData[train_rows,]
# create data.table for testing purposes
DT.test <- DT.rawData[-train_rows,]



####################################  Data - analysis ####################################

# calculate all column means, based on is_spam_flag
DT.col_means <- DT.rawData[, lapply(.SD, mean), by = is_spam_flag]
# bring the values into a "column-based" format for plotting
DT.col_means <- melt(DT.col_means, id.vars = c("is_spam_flag"),value.name = 'mean')

# plot all means for the variables starting with "word_*"
ggplot(data = DT.col_means[DT.col_means$variable %like% 'word',] ,
       aes(x = variable, y = mean, color = is_spam_flag)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Beschriftung x-Achse um 90° drehen
        axis.title.x = element_blank()) + # delete x-labels
  scale_color_manual(values=c("green", "red")) +
  labs(color = "Spam") # Legende beschriften

# plot all means for the variables starting with "char_*"
ggplot(data = DT.col_means[DT.col_means$variable %like% 'char',] ,
       aes(x = variable, y = mean, color = is_spam_flag)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Beschriftung x-Achse um 90° drehen
        axis.title.x = element_blank()) + # delete x-labels
  scale_color_manual(values=c("green", "red")) +
  labs(color = "Spam") # Legende beschriften

# plot all means for the variables starting with "cap_*"
ggplot(data = DT.col_means[DT.col_means$variable %like% 'cap',] ,
       aes(x = variable, y = mean, color = is_spam_flag)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Beschriftung x-Achse um 90° drehen
        axis.title.x = element_blank()) + # delete x-labels
  scale_color_manual(values=c("green", "red")) +
  labs(color = "Spam") # Legende beschriften


# calculate the correlation between all columns except the Spam-flag
corrplot(cor(DT.rawData[,.SD, .SDcols = DT.header$V1]), method = "ellipse")
# "zoom-in" and just take columns with "higher" correlation
corrplot(cor(DT.rawData[,.SD, .SDcols = c(which(DT.header$V1 == 'word_freq_money') : which(DT.header$V1 == 'word_freq_cs'),
                                          which(DT.header$V1 == 'char_freq_#') : which(DT.header$V1 == 'capital_run_length_total'))]),
         method = "ellipse")


############# continue here with better code ######################## 

boxplot(DT.rawData[,-c(55:58)])
hist(log(DT.rawData$word_freq_make))
hist(log(DT.rawData$word_freq_address))

densityplot(log(DT.rawData$word_freq_make))
summary(DT.rawData[,])


par(mfrow = c(60,1))
apply(DT.rawData, 2, hist)

str(d)

str(DT.train)

head(DT.rawData)

table(DT.rawData$is_spam_flag)

cor(DT.train[,1:10])
corrplot(cor(DT.train[,24:41]), method = "ellipse")

