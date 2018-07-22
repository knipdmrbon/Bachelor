# load all required packages
library(rstudioapi) # for setting the working directory to the path of the script
library(data.table)
library(caret)
library(ggplot2)
library(corrplot)


set.seed(100)
# set the working directory to the path of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# read the header of the data
DT.header <- fread("spambase/header.txt", header = FALSE)
# read the raw-data
DT.rawData <- fread("spambase/spambase.data", sep = ",", header = FALSE)
# create header for the data
names(DT.rawData) <- c(as.vector(DT.header$V1), "is_spam_flag")

# Find 90% of data for training
train_rows <- createDataPartition(DT.rawData$is_spam_flag, p = 0.9, list = FALSE)
# create data.table for training
DT.train <- DT.rawData[train_rows,]
# create data.table for testing
DT.test <- DT.rawData[-train_rows,]

# calculate all column means, based on is_spam_flag
col_means <- DT.rawData[, lapply(.SD, mean), by = is_spam_flag]

col_means <- melt(col_means, id.vars = c("is_spam_flag"),value.name = 'mean')



####################################  Data - analysis ####################################

col_means$is_spam_flag <- factor(col_means$is_spam_flag, levels = c(0,1), labels = c("No", "Yes"))
col_means_plot <- col_means[!grepl("capital", col_means$variable),]

ggplot(data = col_means_plot, aes(x=variable,y=mean,color = col_means_plot$is_spam_flag)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Beschriftung x-Achse um 90° drehen
  scale_color_manual(values=c("green", "red")) +
  labs(color = "Spam") # Legende beschriften

# plot correlation
corrplot(cor(DT.rawData[,-58]), method = "ellipse") # take out the 0/1 response 
corrplot(cor(DT.rawData[,c(24:41, 55:57)]), method = "ellipse") # "zoom-in" just take columns with correlation

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

